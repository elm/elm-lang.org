{-# LANGUAGE OverloadedStrings #-}
module Generate
  ( Analytics(..)
  , Highlight(..)
  , serverHtml, compilerSuccess, compilerError
  )
  where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



data Analytics = Analytics | NoAnalytics
data Highlight = Highlight | NoHighlight


-- LOCAL HTML


serverHtml :: String -> String -> H.Html
serverHtml name jsSource =
  htmlSkeleton Analytics Highlight name $
    do  H.script $ Blaze.preEscapedToMarkup jsSource
        H.script "var runningElmModule = Elm.Main.fullscreen();"



-- FOREIGN HTML


compilerSuccess :: String -> String -> H.Html
compilerSuccess moduleName jsSource =
  htmlSkeleton NoAnalytics NoHighlight moduleName $
    do  H.script $ Blaze.preEscapedString jsSource
        H.script $ Blaze.preEscapedString $
          "var runningElmModule = Elm." ++  moduleName ++ ".fullscreen();"


compilerError :: String -> H.Html
compilerError errorJson =
  htmlSkeleton NoAnalytics Highlight "Oops!" $
    do  H.script ! A.src "/editor/errors.js" $ ""
        H.script $ Blaze.string (initErrorScreen errorJson)


initErrorScreen :: String -> String
initErrorScreen errorJson =
  "var errors = Elm.Errors.fullscreen("
  ++ BS.toString (Json.encode errorJson)
  ++ ");"



-- CREATE HTML DOCUMENTS


htmlSkeleton :: Analytics -> Highlight -> String -> H.Html -> H.Html
htmlSkeleton analytics highlight title scripts =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title (H.toHtml title)
      favicon
      H.link ! A.rel "stylesheet" ! A.href "/assets/style.css?v=4"

      case analytics of
        Analytics ->
          googleAnalytics

        NoAnalytics ->
          return ()

      case highlight of
        Highlight ->
          do  H.link ! A.rel "stylesheet" ! A.href "/assets/highlight/styles/default.css"
              H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""

        NoHighlight ->
          return ()

    H.body scripts


favicon :: H.Html
favicon =
  H.link
    ! A.rel "shortcut icon"
    ! A.sizes "16x16 32x32 48x48 64x64 128x128 256x256"
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
