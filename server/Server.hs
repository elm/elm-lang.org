{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head, span, id, catch)
import Control.Monad (msum, zipWithM_)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Happstack.Server hiding (body)
import Happstack.Server.Compression

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

import DirTree
import qualified Language.Elm as Elm (docs)
import ElmToHtml
import Editor
import Utils

-- | Set up the server.
main :: IO ()
main = simpleHTTP nullConf $ do
         docsPath <- liftIO $ Elm.docs
         compressedResponseFilter
         msum [ nullDir >> compileFile "Elm.elm"
              , serveDirectory DisableBrowsing [] "resources"
              , dir "try" (ok $ toResponse $ emptyIDE)
              , dir "compile" $ compilePart (elmToHtml "Compiled Elm")
              , dir "hotswap" $ compilePart elmToJS
              , dir "jsondocs" $ serveFile (asContentType "text/json") docsPath
              , dir "edit" . uriRest $ withFile ide
              , dir "code" . uriRest $ withFile editor
              , dir "login" sayHi
              , uriRest compileFile
              ]

-- | Compile an Elm program that has been POST'd to the server.
compilePart compile = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  code <- look "input"
  ok $ toResponse $ compile code

open :: String -> ServerPart (Maybe String)
open fp = do exists <- liftIO (doesFileExist file)
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
    Nothing -> do
      content <- liftIO (readFile "public/Error404.elm")
      notFound . toResponse $ elmToHtml (pageTitle fp) content

-- | Compile an arbitrary Elm program from the public/ directory.
compileFile :: FilePath -> ServerPart Response
compileFile = withFile elmToPage

-- | Compile an elm file with a page title.
elmToPage :: FilePath -> String -> Html
elmToPage = elmToHtml . pageTitle

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
compileAll :: FilePath -> FilePath -> IO ()
compileAll fromPre toPre = do
  dirTree <- buildDirTree fromPre
  let fromToTo =  map ((toPre </>) . fromJust . stripPrefix fromPre)
      compDirs =  fromToTo (directories dirTree)
      compFiles = fromToTo (files dirTree)
  mapM_ (createDirectoryIfMissing True) compDirs
  zipWithM_ compileFromTo (files dirTree) compFiles
  where -- pubPrefix = "public/"
        -- compPrefix = "compiled/"

compileFromTo :: FilePath -> FilePath -> IO ()
compileFromTo from to = do
  contents <- readFile from
  let out = renderHtml $ elmToPage to contents
  writeFile to out
