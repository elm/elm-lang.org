{-# LANGUAGE OverloadedStrings #-}
module Editor (editor,ide) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import Utils

-- | Display an editor and the compiled result side-by-side.
ide :: FilePath -> String -> Html
ide fileName code =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ pageTitle fileName
        googleAnalytics
      preEscapedToMarkup $ 
         concat [ "<frameset cols=\"50%,50%\">\n"
                , "  <frame name=\"input\" src=\"/code/" ++ fileName ++ "\" />\n"
                , "  <frame name=\"output\" src=\"/compile?input="
                , urlEncode code ++ "\" />\n</frameset>" ]


-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: FilePath -> String -> Html
editor filePath code =
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ pageTitle filePath
        H.link ! A.rel "stylesheet" ! A.href "/CodeMirror-2.13/lib/codemirror.css"
        H.script ! A.src "/CodeMirror-2.13/lib/codemirror.js" $ mempty
        H.script ! A.src "/CodeMirror-2.13/mode/haskell/haskell.js" $ mempty
        H.link ! A.rel "stylesheet" ! A.href "/CodeMirror-2.13/theme/cobalt.css"
        H.style ! A.type_ "text/css" $ editorCss
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
        H.script ! A.type_ "text/javascript" $ editorJS

-- | CSS needed to style the CodeMirror frame.
editorCss :: Markup
editorCss = preEscapedToMarkup $
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

-- | JS needed to set up CodeMirror.
editorJS :: Html
editorJS =
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
