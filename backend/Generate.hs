{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate (html, js) where

import Control.Exception (evaluate, catches, Handler(..), SomeException)
import Control.Monad    (forM_, when)
import Data.Functor     ((<$>))
import Data.Maybe       (fromMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.IO (openTempFile, hPutStr, hClose)
import System.Process
import Text.Blaze       (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Internal.Utils as Elm
import Elm.Internal.Version (elmVersion)
import Utils


-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
html :: String -> String -> IO H.Html
html name src =
  do  compilerResult <- safeCompile src
      return . buildPage $ formatResult compilerResult
  where
    script = H.script ! A.type_ "text/javascript"

    formatResult compilerResult =
        case compilerResult of
          Right jsSrc ->
              do script $ preEscapedToMarkup jsSrc
                 script $ preEscapedToMarkup $ runFullscreen
          Left err ->
              H.span ! A.style "font-family: monospace;" $
               forM_ (lines err) $ \line ->
                   do preEscapedToMarkup (addSpaces line)
                      H.br

    runFullscreen =
        let moduleName = "Elm." ++ fromMaybe "Main" (Elm.moduleName src)
        in  "var runningElmModule = Elm.fullscreen(" ++  moduleName ++ ")"

    buildPage content = H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.charset "UTF-8"
          H.title . H.toHtml $ name
          H.style ! A.type_ "text/css" $ preEscapedToMarkup
              ("a:link {text-decoration: none; color: rgb(15,102,230);}\n\
               \a:visited {text-decoration: none; color: rgb(15,102,230);}\n\
               \a:active {text-decoration: none}\n\
               \a:hover {text-decoration: underline; color: rgb(234,21,122);}" :: String)
        H.body $ do
          script ! A.src (H.toValue ("/elm-runtime.js?" ++ show elmVersion)) $ ""
          content
        googleAnalytics


addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest -> " &nbsp;" ++ addSpaces rest
    c : rest -> c : addSpaces rest
    [] -> []


js :: String -> IO String
js src =
  do output <- safeCompile src
     return (either (wrap "error") (wrap "success") output)
  where
    wrap :: String -> String -> String
    wrap typ msg = "{ " ++ show typ ++ " : " ++ show msg ++ " }"


safeCompile :: String -> IO (Either String String)
safeCompile src =
  case Elm.nameAndImports src of
    Nothing -> compileNormal src
    Just (_name, imports) ->
        if any (`elem` thirdPartyLibraries) imports
          then compileInSandbox src
          else compileNormal src


thirdPartyLibraries :: [String]
thirdPartyLibraries =
    [ "Graphics.WebGL"
    , "Math.Matrix4"
    , "Math.Vector2"
    , "Math.Vector3"
    , "Math.Vector4"
    ]

compileNormal :: String -> IO (Either String String)
compileNormal src =
  do output <- catchBugs (Elm.compile src)
     return (either explain id output)
  where
    explain problem = Left ("Error:\n" ++ problem)

catchBugs :: a -> IO (Either String a)
catchBugs inp = (Right <$> evaluate inp) `catches` handlers
  where
    handlers = [ Handler (return . Left . show :: SomeException -> IO (Either String b)) ]

compileInSandbox :: String -> IO (Either String String)
compileInSandbox src =
  do (file, handle) <- openTempFile "." "Temp.elm"
     hPutStr handle src
     hClose handle
     (exitCode, stdout, stderr) <- readProcessWithExitCode "elm" (args file) ""
     case exitCode of
       ExitFailure _ ->
           do removeEverything file
              return (Left (stdout ++ stderr))
       ExitSuccess ->
           do result <- readFile ("build" </> file `replaceExtension` "js")
              length result `seq` removeEverything file
              return (Right result)
  where
    args file =
        [ "--make"
        , "--only-js"
        , file
        ]

    removeEverything :: FilePath -> IO ()
    removeEverything file =
        do remove "." "elm"
           remove "cache" "elmi"
           remove "cache" "elmo"
           remove "build" "js"
        where
          remove :: String -> String -> IO ()
          remove dir ext = do
            let path = dir </> file `replaceExtension` ext
            exists <- doesFileExist path
            when exists (removeFile path)
