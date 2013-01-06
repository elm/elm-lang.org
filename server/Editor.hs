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
themes = [ "ambiance", "blackboard", "cobalt", "eclipse"
         , "elegant", "erlang-dark", "lesser-dark", "monokai", "neat", "night"
         , "rubyblue", "solarized", "twilight", "vibrant-ink", "xq-dark" ]

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
        H.script ! A.type_ "text/javascript" ! A.src "/misc/editor.js" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "editor_box" ! A.style "position:absolute;top:0;left:0;right:0;bottom:36px;" $ do
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.class_ "opts" ! A.id "options" $ do
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
             H.div ! A.style "float:left; padding:10px;" $ do
               H.span ! A.title "Show the basic examples" $ do
                  H.span ! A.class_ "valign" $ "Examples:"
                  H.input ! A.class_ "valign" ! A.type_ "checkbox" !
                     A.onchange "window.top.toggleExamples(this.checked);"
               H.span ! A.style "padding-left: 16px;" ! A.class_ "valign" $
                    "Options:"
               H.input ! A.class_ "valign" ! A.type_ "checkbox" !
                  A.onchange "toggleOptions(this.checked);"
           H.div ! A.class_ "opts" ! A.id "editor_options" $ do
             let optionFor text =
                   H.option ! A.value (toValue (text :: String)) $
                   toHtml text
             H.select ! A.class_ "valign" ! A.id "editor_theme" !
               A.onchange "setTheme()" $ mapM_ optionFor themes
             H.select ! A.class_ "valign" ! A.id "editor_zoom" !
               A.onchange "setZoom()" $ mapM_ optionFor ["100%", "80%", "150%", "200%"]
             H.span $ do
               H.span ! A.class_ "valign" $ " Line Numbers:"
               H.input ! A.class_ "valign" ! A.id "editor_lines" !
                 A.type_ "checkbox" !
                 A.onchange "toggleLines(this.checked);"
        H.script ! A.type_ "text/javascript" $ editorJS

-- | CSS needed to style the CodeMirror frame.
editorCss :: Markup
editorCss = preEscapedToMarkup $
    ("body { margin: 0; }\n\
     \.CodeMirror { height: 100% }\n\
     \form { margin-bottom: 0; }\n\
     \.zoom-80 { font-size: 80%; }\n\
     \.zoom-100 { font-size: 100%; }\n\
     \.zoom-150 { font-size: 150%; }\n\
     \.zoom-200 { font-size: 200%; }\n\
     \.valign { vertical-align: middle; }\n\
     \.opts {\n\
     \  background-color: rgb(216,221,225);\n\
     \  font-family: Arial;\n\
     \  font-size: 14px;\n\
     \  overflow: hidden;\n\
     \  position: absolute;\n\
     \}\n\
     \#editor_options {\n\
     \  bottom: 28px;\n\
     \  left: 0;\n\
     \  right: 0;\n\
     \  padding: 4px;\n\
     \  visibility: hidden;\n\
     \}\n\
     \#options {\n\
     \  left: 0;\n\
     \  right: 0;\n\
     \  bottom: 0;\n\
     \  height: 36px;\n\
     \}" :: String)

-- | JS needed to set up CodeMirror.
editorJS :: Html
editorJS =
    "var editor = CodeMirror.fromTextArea(document.getElementById('input'), {\n\
    \ lineNumbers: initLines(),\n\
    \ matchBrackets: true,\n\
    \ theme: initTheme(),\n\
    \ tabMode: 'shift',\n\
    \ extraKeys: {'Ctrl-Enter': compileOutput},\n\
    \});\n\
    \editor.focus();\n\
    \initZoom();"
