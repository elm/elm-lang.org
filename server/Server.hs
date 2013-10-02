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
import GHC.Conc

import qualified Language.Elm as Elm
import ElmToHtml
import Editor
import Utils

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  putStrLn "Initializing Server"
  precompile
  getRuntime
  putStrLn "Serving at localhost:8000"
  simpleHTTP nullConf $ do
    compressedResponseFilter
    let mime = asContentType "text/html; charset=UTF-8"
    route (serveFile mime "public/build/Elm.elm")
          (serveDirectory' EnableBrowsing [] mime "public/build")

route :: ServerPartT IO Response -> ServerPartT IO Response -> ServerPartT IO Response
route empty rest = do
  msum [ nullDir >> empty
       , serveDirectory DisableBrowsing [] "resources"
       , dir "try" (ok $ toResponse $ emptyIDE)
       , dir "compile" $ compilePart (elmToHtml "Compiled Elm")
       , dir "hotswap" $ compilePart elmToJS
       , dir "jsondocs" $ serveFile (asContentType "text/json") "resources/docs.json?v0.9"
       , dir "edit" serveEditor
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
    file = "public/" ++ takeWhile (/='?') fp
    openFile = liftIO $ catch (fmap Just $ readFile file) handleError

    handleError :: SomeException -> IO (Maybe String)
    handleError _ = return Nothing

serveEditor :: ServerPart Response
serveEditor = do
  cols <- getDataFn (look "cols")
  uriRest . withFile . ide $ case cols of
                               Left _    -> "50%,50%"
                               Right str -> str

-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  eitherContent <- open fp
  case eitherContent of
    Just content -> ok . toResponse $ handler fp content
    Nothing -> return404

return404 =
  notFound =<< serveFile (asContentType "text/html; charset=UTF-8") "public/build/Error404.elm"

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

-- | Compile all of the Elm files in public/, placing results in public/build/
precompile :: IO ()
precompile =
  do setCurrentDirectory "public"
     files <- getFiles True ".elm" "."
     forM_ files $ \file -> do
       rawSystem "elm" ["--make","--runtime=/elm-runtime.js?v0.9",file]
     htmls <- getFiles False ".html" "build"
     mapM_ adjustHtmlFile htmls
     setCurrentDirectory ".."
  where
    getFiles :: Bool -> String -> FilePath -> IO [FilePath]
    getFiles skip ext dir = do
        case skip && "build" `elem` map dropTrailingPathSeparator (splitPath dir) of
          True -> return []
          False -> do
            contents <- map (dir </>) `fmap` getDirectoryContents dir
            let files = filter ((ext==) . takeExtension) contents
                dirs  = filter (not . hasExtension) contents
            filess <- mapM (getFiles skip ext) dirs
            return (files ++ concat filess)

getRuntime :: IO ()
getRuntime = do
  rts <- readFile =<< Elm.runtime
  writeFile "resources/elm-runtime.js" rts

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- readFile file
     let (before,after) =
             length src `seq`
             List.break (List.isInfixOf "<title>") (lines src)
     removeFile file
     writeFile (replaceExtension file "elm") (unlines (before ++ [style] ++ after ++ [renderHtml googleAnalytics]))
  where
    style = 
        unlines . map ("    "++) $
        [ "<style type=\"text/css\">"
        , "  a:link {text-decoration: none; color: rgb(15,102,230);}"
        , "  a:visited {text-decoration: none}"
        , "  a:active {text-decoration: none}"
        , "  a:hover {text-decoration: underline; color: rgb(234,21,122);}"
        , "  body { font-family: \"Lucida Grande\",\"Trebuchet MS\",\"Bitstream Vera Sans\",Verdana,Helvetica,sans-serif !important; }"
        , "  p, li { font-size: 14px !important;"
        , "          line-height: 1.5em !important; }"
        , "</style>" ]
