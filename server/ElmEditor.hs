{-# LANGUAGE OverloadedStrings #-}
module ElmEditor (editor, compile, codeFrame) where

import Data.Monoid (mempty)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)

import Language.Elm (toParts)

pageTitle fp = reverse . takeWhile (/='/') . drop 4 $ reverse fp

editor :: String -> String -> Html
editor fileName code =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ pageTitle fileName
        googleAnalytics
      preEscapedToMarkup $ frameset fileName code

codeCss = preEscapedToMarkup $
    ("body { margin: 0; }\n\
     \.CodeMirror-scroll { height: 100%; }\n\
     \.activeline {background: #00162a !important;}\n\
     \form { margin-bottom: 0; }\n\
     \#compile_options {\n\
     \   position: fixed;\n\
     \   bottom: 0;\n\
     \   right: 0;\n\
     \   background-color: rgb(208,208,208);//#b5b7b8;\n\
     \   padding: 4px;\n\
     \   font-family: Arial;\n\
     \   font-size: 14px;\n\
     \}" :: String)

codeFrame :: String -> String -> Html
codeFrame fileName code =
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ pageTitle fileName
        H.link ! A.rel "stylesheet" ! A.href "/CodeMirror-2.13/lib/codemirror.css"
        H.script ! A.src "/CodeMirror-2.13/lib/codemirror.js" $ mempty
        H.script ! A.src "/CodeMirror-2.13/mode/haskell/haskell.js" $ mempty
        H.link ! A.rel "stylesheet" ! A.href "/CodeMirror-2.13/theme/cobalt.css"
        H.style ! A.type_ "text/css" $ codeCss
        H.script ! A.type_ "text/javascript" $
                "function compile(formTarget) {\n\
                \  var form = document.getElementById('inputForm');\n\
                \  form.target = formTarget;\n\
                \  form.submit();\n\
                \};"                 
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
               H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n' : code)
               H.div ! A.id "compile_options" $ do
                 "Compile: "
                 H.input ! A.type_ "button" ! A.onclick "compile('output')" ! A.value "Side-By-Side"
                 H.input ! A.type_ "button" ! A.onclick "compile('_blank')" ! A.value "New Tab"
        H.script ! A.type_ "text/javascript" $
                   "var editor = CodeMirror.fromTextArea(document.getElementById('input'), {\
                   \lineNumbers: false,\
                   \matchBrackets: true,\
                   \theme: 'cobalt',\
                   \tabMode: 'shift',\
                   \onCursorActivity: function() {\
                   \editor.setLineClass(hlLine, null);\
                   \hlLine = editor.setLineClass(editor.getCursor().line, 'activeline');\
                   \}\
                   \});\
                   \var hlLine = editor.setLineClass(0, 'activeline');\
                   \editor.focus();"

frameset fileName code =
    "<frameset cols=\"50%,50%\">\n" ++
    "  <frame name=\"input\" src=\"/code/" ++ fileName ++ "\" />\n" ++
    "  <frame name=\"output\" src=\"/compile?input=" ++ urlEncode code ++ "\" />\n" ++
    "</frameset>"

compile :: String -> String -> H.Html
compile name src =
  let (body, css, js) = toParts src in
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ name
        css
      H.body $ do
        body
        H.script ! A.type_ "text/javascript" ! A.src (H.toValue ("/elm-mini.js" :: String)) $ ""
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup js
        H.script ! A.type_ "text/javascript" $ "Dispatcher.initialize()"
        googleAnalytics

googleAnalytics =
    H.script ! A.type_ "text/javascript" $
               "var _gaq = _gaq || [];\n\
               \_gaq.push(['_setAccount', 'UA-25827182-1']);\n\
               \_gaq.push(['_trackPageview']);\n\
               \(function() {\n\
               \  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
               \  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
               \  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
               \})();"


{--
  ,\
  \onChange: function(e,change) {\
  \if (change.from.ch === change.to.ch && change.from.line === change.to.line) {\
  \var n = change.from.line;\
  \if (change.text[0] === \">\") {\
  \e.setLine(n, e.getLine(n).replace(/->/g,\"\\u2192\"));\n\
  \e.setCursor(change.to.line, change.to.ch);\
  \}\n\
  \if (change.text[0] === \"\\\\\") {\n\
  \e.setLine(n, e.getLine(n).replace(/\\\\/g,\"\\u03BB\"));\n\
  \e.setCursor(change.to.line, change.to.ch+1);\
  \}}}
--}