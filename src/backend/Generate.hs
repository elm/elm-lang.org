{-# LANGUAGE OverloadedStrings #-}
module Generate (serverHtml, userHtml, js) where

import Control.Monad (forM_, when)
import qualified Data.Text as Text
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- JS

js :: Either String (String,String) -> String
js result =
  case result of
    Left msg ->
        "{ \"error\": " ++ show msg ++ " }"

    Right (moduleName, jsSource) ->
        "{ \"name\": " ++ show moduleName ++
        ", \"success\": " ++ show jsSource ++
        " }"


-- HTML

serverHtml :: String -> Text.Text -> H.Html
serverHtml name jsSource =
    htmlSkeleton False name $
      do  H.script $ preEscapedToMarkup jsSource
          H.script "var runningElmModule = Elm.fullscreen(Elm.Main);"


userHtml :: Either String (String, String) -> H.Html
userHtml compilerResult =
  case compilerResult of
    Right (moduleName, jsSource) ->
        htmlSkeleton True moduleName (scripts moduleName jsSource)

    Left err ->
        htmlSkeleton True "Oops!" $
            H.span ! A.style "font-family: monospace;" $
                forM_ (lines err) $ \line ->
                    do  preEscapedToMarkup (addSpaces line)
                        H.br


scripts :: H.ToMarkup a => String -> a -> H.Html
scripts moduleName jsSource =
  do  H.script ! A.src "/editor/everything.js" $ ""
      H.script $ preEscapedToMarkup jsSource
      H.script $ preEscapedToMarkup $
          "var runningElmModule = Elm.fullscreen(Elm." ++  moduleName ++ ");"


addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest -> " &nbsp;" ++ addSpaces rest
    c : rest -> c : addSpaces rest
    [] -> []


-- CREATE HTML DOCUMENTS

htmlSkeleton :: Bool -> String -> H.Html -> H.Html
htmlSkeleton userGenerated title scripts =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      favicon
      H.style $ preEscapedToMarkup standardStyle
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


standardStyle :: Text.Text
standardStyle =
    "html,head,body { padding:0; margin:0; }\n\
    \body { font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\n\
    \a {\n\
    \  color: #1184CE;\n\
    \  text-decoration: none;\n\
    \}\n\
    \a:hover {\n\
    \  text-decoration: underline;\n\
    \  color: rgb(234,21,122);\n\
    \}\n\
    \h1,h2,h3,h4 { font-weight:normal; font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; }\n\
    \p, li {\n\
    \  font-size: 14px !important;\n\
    \  line-height: 1.5em !important;\n\
    \}\n\
    \pre {\n\
    \  margin: 0;\n\
    \  padding: 10px;\n\
    \  background-color: rgb(254,254,254);\n\
    \  border-style: solid;\n\
    \  border-width: 1px;\n\
    \  border-color: rgb(245,245,245);\n\
    \  border-radius: 6px;\n\
    \}\n"