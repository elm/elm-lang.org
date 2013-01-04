{-# LANGUAGE OverloadedStrings #-}
module Editor (editor,ide,emptyIDE) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import Utils

-- | Display an editor and the compiled result side-by-side.
ide :: FilePath -> String -> Html
ide fileName code = ideBuilder ("Elm Editor: " ++ pageTitle fileName)
                               fileName
                               ("/compile?input=" ++ urlEncode code)

-- | Display an editor and the compiled result side-by-side.
emptyIDE :: Html
emptyIDE = ideBuilder "Try Elm" "" "/Try.elm"

ideBuilder :: String -> String -> String -> Html
ideBuilder title input output =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ title
        H.script ! A.type_ "text/javascript" $
             "function toggleExamples(open) {\n\
             \  document.getElementById('frameset1').rows = open ? '*,160px' : '*,0';\n\
             \};"
      preEscapedToMarkup $ 
         concat [ "<frameset id=\"frameset1\" rows=\"*,0\">\n"
                , "  <frameset cols=\"50%,50%\">\n"
                , "    <frame name=\"input\" src=\"/code/", input, "\" />\n"
                , "    <frame name=\"output\" src=\"", output, "\" />\n"
                , "  </frameset>\n"
                , "  <frame src=\"/examples/Navigation.elm\" />\n"
                , "</frameset>" ]

-- | list of themes to use with CodeMirror
themes = [ "cobalt", "night", "elegant", "default" ]

-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: FilePath -> String -> Html
editor filePath code =
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ pageTitle filePath
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-3.0/lib/codemirror.css"
        H.script ! A.src "/codemirror-3.0/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-3.0/mode/elm/elm.js" $ mempty
        mapM_ (\theme -> H.link ! A.rel "stylesheet" ! A.href (toValue ("/codemirror-3.0/theme/" ++ theme ++ ".css" :: String))) themes
        H.style ! A.type_ "text/css" $ editorCss
        H.script ! A.type_ "text/javascript" $
                "function compile(formTarget) {\n\
                \  var form = document.getElementById('inputForm');\n\
                \  form.target = formTarget;\n\
                \  form.submit();\n\
                \};\n\
                \var delay;\
                \function toggleAutoUpdate(enable) {\n\
                \  document.getElementById('compile_button').disabled = enable;\n\
                \  if (enable) {\n\
                \    editor.on('change', updateOutput);\n\
                \  } else {\n\
                \    editor.off('change', updateOutput);\n\
                \  }\n\
                \};\n\
                \function updateOutput() {\
                \  clearTimeout(delay);\
                \  delay = setTimeout(compileOutput, 1000);\
                \};\n\
                \function compileOutput() {\
                \  compile('output');\
                \};\n\
                \function setTheme() {\n\
                \  var input = document.getElementById('editor_theme');\n\
                \  var theme = input.options[input.selectedIndex].innerHTML;\n\
                \  editor.setOption('theme', theme);\n\
                \};\n\
                \function setZoom() {\n\
                \  var editorDiv = document.getElementsByClassName('CodeMirror')[0];\n\
                \  var classes = editorDiv.getAttribute('class').split(' ');\n\
                \  var input = document.getElementById('editor_zoom');\n\
                \  var zoom = 'zoom-' + input.options[input.selectedIndex].innerHTML.slice(0,-1);\n\
                \  var newClasses = [];\n\
                \  for (var i = 0; i < classes.length; i++)\n\
                \    if (!(classes[i].match(/^zoom-/)))\n\
                \      newClasses.push(classes[i]);\n\
                \  newClasses.push(zoom);\n\
                \  editorDiv.setAttribute('class', newClasses.join(' '));\n\
                \};"
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
               H.div ! A.style "position:absolute;top:0;left:0;right:0;bottom:36px;" $ do
                 H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
               H.div ! A.id "options" $ do
                 H.div ! A.style "float:right; padding:6px;" $ do
                   H.input ! A.class_ "valign" !
                      A.id "compile_button" ! A.type_ "button" !
                      A.onclick "compileOutput()" ! A.value "Compile" !
                      A.title "... or hit Ctrl+Enter"
                   H.input ! A.class_ "valign" !
                      A.id "in_tab_button" ! A.type_ "button" !
                      A.onclick "compile('_blank')" ! A.value "In Tab" !
                      A.title "compile in a new tab"
                   H.span  ! A.class_ "valign" $ " Auto-compile:"
                   H.input ! A.class_ "valign" ! A.type_ "checkbox" !
                      A.onchange "toggleAutoUpdate(this.checked)"
                 H.div ! A.style "float:left; padding:6px;" $ do
                   let optionFor text = H.option ! A.value (toValue (text :: String)) $ toHtml text
                   H.select ! A.class_ "valign" ! A.id "editor_theme" !
                      A.onchange "setTheme()" $ mapM_ optionFor themes
                   H.select ! A.class_ "valign" ! A.id "editor_zoom" !
                      A.onchange "setZoom()" $ mapM_ optionFor ["100%", "150%", "200%"]
                   H.span ! A.title "Show the basic examples" $ do
                      H.span ! A.class_ "valign" $ " Examples:"
                      H.input ! A.class_ "valign" ! A.type_ "checkbox" !
                         A.onchange "window.top.toggleExamples(this.checked);"

        H.script ! A.type_ "text/javascript" $ editorJS

-- | CSS needed to style the CodeMirror frame.
editorCss :: Markup
editorCss = preEscapedToMarkup $
    ("body { margin: 0; }\n\
     \.CodeMirror { height: 100% }\n\
     \form { margin-bottom: 0; }\n\
     \.zoom-100 { font-size: 100%; }\n\
     \.zoom-150 { font-size: 150%; }\n\
     \.zoom-200 { font-size: 200%; }\n\
     \.valign { vertical-align: middle; }\n\
     \#options {\n\
     \  background-color: rgb(216,221,225);\n\
     \  font-family: Arial;\n\
     \  font-size: 14px;\n\
     \  overflow: hidden;\n\
     \  position: absolute;\n\
     \  left: 0;\n\
     \  right: 0;\n\
     \  bottom: 0;\n\
     \  height: 36px;\n\
     \}" :: String)

-- | JS needed to set up CodeMirror.
editorJS :: Html
editorJS =
    "var editor = CodeMirror.fromTextArea(document.getElementById('input'), {\
      \lineNumbers: false,\
      \matchBrackets: true,\
      \theme: 'cobalt',\
      \extraKeys: {'Ctrl-Enter': compileOutput},\
    \});\
    \editor.focus();"
