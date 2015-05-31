{-# LANGUAGE OverloadedStrings #-}
module Generate (serverHtml, userHtml, js) where

import Control.Monad (when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- JS

js :: Either Json.Value (String,String) -> Json.Value
js result =
  Json.object $
  case result of
    Left msg ->
        [ "error" .= msg ]

    Right (moduleName, jsSource) ->
        [ "name" .= moduleName
        , "success" .= jsSource
        ]


-- HTML

serverHtml :: String -> String -> H.Html
serverHtml name jsSource =
    htmlSkeleton False name $
      do  H.script $ Blaze.preEscapedToMarkup jsSource
          H.script "var runningElmModule = Elm.fullscreen(Elm.Main);"


userHtml :: Either Json.Value (String, String) -> H.Html
userHtml compilerResult =
  case compilerResult of
    Right (moduleName, jsSource) ->
        htmlSkeleton True moduleName (scripts moduleName jsSource)

    Left err ->
        htmlSkeleton True "Oops!" $
          do  H.script ! A.src "/editor/errors.js" $ ""
              H.script $ Blaze.toMarkup (errorJs err)


errorJs :: Json.Value -> String
errorJs err =
  "var textarea = self.parent.input.document.getElementById('input');\n\
  \Elm.fullscreen(Elm.Errors, {\n\
  \    sourceCode: textarea.value,\n\
  \    errors: " ++ LBS.unpack (Json.encode err) ++ "\n\
  \});"


scripts :: H.ToMarkup a => String -> a -> H.Html
scripts moduleName jsSource =
  do  H.script ! A.src "/editor/everything.js" $ ""
      H.script $ Blaze.preEscapedToMarkup jsSource
      H.script $ Blaze.preEscapedToMarkup $
          "var runningElmModule = Elm.fullscreen(Elm." ++  moduleName ++ ");"


-- CREATE HTML DOCUMENTS

htmlSkeleton :: Bool -> String -> H.Html -> H.Html
htmlSkeleton userGenerated title scripts =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      favicon
      H.link ! A.rel "stylesheet" ! A.href "/assets/style.css"
      when (not userGenerated) $
        do  googleAnalytics
            H.link ! A.rel "stylesheet" ! A.href "/highlight/styles/default.css"
            H.script ! A.src "/highlight/highlight.pack.js" $ ""

    H.body scripts


favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.size "16x16, 32x32, 48x48, 64x64, 128x128, 256x256"
    ! A.href "/favicon.ico"


googleAnalytics :: H.Html
googleAnalytics =
    H.script ! A.type_ "text/javascript" $
        "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\n\
        \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\n\
        \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\n\
        \})(window,document,'script','//www.google-analytics.com/analytics.js','ga');\n\
        \\n\
        \ga('create', 'UA-25827182-1', 'auto');\n\
        \ga('send', 'pageview');\n"
