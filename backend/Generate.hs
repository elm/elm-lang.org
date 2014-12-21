{-# LANGUAGE OverloadedStrings #-}
module Generate (htmlSkeleton, html, js, scripts) where

import Control.Monad (forM_, when)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Compile
import qualified Utils


-- CREATE HTML DOCUMENTS

htmlSkeleton :: Bool -> String -> H.Html -> H.Html
htmlSkeleton userGenerated title scripts =
  H.docTypeHtml $ do 
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      H.style $ preEscapedToMarkup Utils.standardStyle
      when (not userGenerated) $
        do  Utils.googleAnalytics
            H.link ! A.rel "stylesheet" ! A.href "/highlight/styles/default.css"
            H.script ! A.src "/highlight/highlight.pack.js" $ ""

    H.body scripts

-- USER-GENERATED ELM TO HTML

html :: String -> String -> IO H.Html
html title source =
  do  compilerResult <- Compile.toJS source
      let htmlChunk = resultToHtml compilerResult
      return (htmlSkeleton True title htmlChunk)


resultToHtml :: (Either String (String, String)) -> H.Html
resultToHtml compilerResult =
  case compilerResult of
    Right (moduleName, jsSource) ->
      scripts moduleName jsSource

    Left err ->
      H.span ! A.style "font-family: monospace;" $
         forM_ (lines err) $ \line ->
             do  preEscapedToMarkup (addSpaces line)
                 H.br


scripts :: H.ToMarkup a => String -> a -> H.Html
scripts moduleName jsSource =
  do  H.script $ preEscapedToMarkup jsSource
      H.script $ preEscapedToMarkup $
          "var runningElmModule = Elm.fullscreen(Elm." ++  moduleName ++ ")"


addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest -> " &nbsp;" ++ addSpaces rest
    c : rest -> c : addSpaces rest
    [] -> []


-- USER-GENERATED ELM TO JS

js :: String -> IO String
js source =
  do  result <- Compile.toJS source
      case result of
        Left msg ->
          return $ "{ \"error\": " ++ show msg ++ " }"

        Right (_, jsSource) ->
          return $ "{ \"success\": " ++ show jsSource ++ " }"
