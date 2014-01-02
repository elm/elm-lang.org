{-# LANGUAGE OverloadedStrings #-}
module Generate (html, js) where

import Control.Exception
import Control.Monad    (join)
import Data.Functor     ((<$>))
import Data.Maybe       (fromMaybe)
import Text.Blaze       (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Internal.Utils as Elm
import Utils

-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
html :: String -> String -> IO H.Html
html name src = do
  out <- safeCompile src
  let elmname = "Elm." ++ fromMaybe "Main" (Elm.moduleName src)
  return . putOutp $ makeOut out elmname

  where makeOut (Right jsSrc) name = do
            js $ preEscapedToMarkup jsSrc
            js $ preEscapedToMarkup . runFullscreen $ name
        makeOut (Left err) _ = H.span ! A.style "font-family: monospace;" $
          mapM_ (\line -> preEscapedToMarkup (addSpaces line) >> H.br) (lines err)

        runFullscreen name = "var runningElmModule = Elm.fullscreen(" ++ name ++ ")"
        js = H.script ! A.type_ "text/javascript"

        putOutp out = H.docTypeHtml $ do
          H.head $ do
            H.meta ! A.charset "UTF-8"
            H.title . H.toHtml $ name
            H.style ! A.type_ "text/css" $ preEscapedToMarkup
              ("a:link {text-decoration: none; color: rgb(15,102,230);}\n\
               \a:visited {text-decoration: none}\n\
               \a:active {text-decoration: none}\n\
               \a:hover {text-decoration: underline; color: rgb(234,21,122);}" :: String)
          H.body $ do
            js ! A.src (H.toValue ("/elm-runtime.js" :: String)) $ ""
            out
          googleAnalytics

addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest -> " &nbsp;" ++ addSpaces rest
    c : rest -> c : addSpaces rest
    [] -> []

js :: String -> String
js src = case Elm.compile src of
           Right js -> "{ \"success\" : " ++ show js ++ " }"
           Left err -> "{ \"error\" : " ++ show err ++ " }"

catchBugs :: a -> IO (Either String a)
catchBugs inp = (Right <$> evaluate inp) `catches` handlers
  where handlers = [ Handler (return . Left . show :: SomeException -> IO (Either String b)) ]

safeCompile :: String -> IO (Either String String)
safeCompile inp = either explain id <$> (catchBugs . Elm.compile $ inp)
  where explain problem = Left . unlines $ [
          "Elm compiler bug found!"
          , "Please report this as an issue at https://github.com/evancz/Elm/issues including your elm code and the following error message:"
          , show problem
          ]
