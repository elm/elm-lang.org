{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head, span, id, catch)
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Monad
import Happstack.Server hiding (body)
import Happstack.Server.Compression
import Happstack.Server.FileServe.BuildingBlocks (serveDirectory')

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception
import System.FilePath as FP
import System.Process
import System.Directory

import qualified Language.Elm as Elm
import ElmToHtml
import Editor

-- | Set up the server.
main :: IO ()
main = do
  putStrLn "Initializing Server"
  precompile
  getRuntime
  putStrLn "Serving at localhost:8000"
  simpleHTTP nullConf $ do
    compressedResponseFilter
    docsPath <- liftIO $ Elm.docs
    let mime = asContentType "text/html; charset=UTF-8"
    route docsPath (serveFile mime "public/ElmFiles/Elm.elm")
          (serveDirectory' EnableBrowsing [] mime "public/ElmFiles")

route :: FilePath -> ServerPartT IO Response -> ServerPartT IO Response -> ServerPartT IO Response
route docsPath empty rest = do
  msum [ nullDir >> empty
       , serveDirectory DisableBrowsing [] "resources"
       , dir "try" (ok $ toResponse $ emptyIDE)
       , dir "compile" $ compilePart (elmToHtml "Compiled Elm")
       , dir "hotswap" $ compilePart elmToJS
       , dir "jsondocs" $ serveFile (asContentType "text/json") docsPath
       , dir "edit" . uriRest $ withFile ide
       , dir "code" . uriRest $ withFile editor
       , dir "login" sayHi
       , rest
       , return404
       ]

-- | Compile an Elm program that has been POST'd to the server.
compilePart compile = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  code <- look "input"
  ok $ toResponse $ compile code

open :: String -> ServerPart (Maybe String)
open fp =
  do exists <- liftIO (doesFileExist file)
     if exists then openFile else return Nothing
  where
    file = "public/" ++ fp
    openFile = liftIO $ catch (fmap Just $ readFile file) handleError

    handleError :: SomeException -> IO (Maybe String)
    handleError _ = return Nothing


-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  eitherContent <- open fp
  case eitherContent of
    Just content -> ok . toResponse $ handler fp content
    Nothing -> return404

return404 = serveFile (asContentType "text/html; charset=UTF-8") "public/ElmFiles/Error404.elm"

-- | Compile an arbitrary Elm program from the public/ directory.
compileFile :: FilePath -> ServerPart Response
compileFile = withFile (elmToHtml . FP.takeBaseName)

-- | Simple response for form-validation demo.
sayHi :: ServerPart Response
sayHi = do
  first <- look "first"
  last  <- look "last"
  email <- look "email"
  ok . toResponse $
     concat [ "Hello, ", first, " ", last
            , "! Welcome to the fake login-confirmation page.\n\n"
            , "We will not attempt to contact you at ", email
            , ".\nIn fact, your (fake?) email has not even been recorded." ]

-- | Compile all of the Elm files in public/ to the compiled/ folder
precompile :: IO ()
precompile =
  do setCurrentDirectory "public"
     files <- getFiles True "."
     forM_ files $ \file -> do
       rawSystem "elm" ["--make","--runtime=/elm-runtime.js",file]
     files' <- getFiles False "ElmFiles"
     forM_ files' $ \file ->
         case takeExtension file == ".html" of
           True -> renameFile file (replaceExtension file "elm")
           False -> removeFile file
     setCurrentDirectory ".."
  where
    getFiles :: Bool -> FilePath -> IO [FilePath]
    getFiles skip dir = do
        print dir
        if skip && List.isInfixOf "ElmFiles" dir then return [] else
            do contents <- map (dir </>) `fmap` getDirectoryContents dir
               files    <- filterM doesFileExist contents
               let dirs =  filter (not . hasExtension) contents
               filess   <- mapM (getFiles skip) dirs
               return (files ++ concat filess)

getRuntime :: IO ()
getRuntime = do
  rts <- readFile =<< Elm.runtime
  writeFile "resources/elm-runtime.js" rts