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
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-3.x/lib/codemirror.css"
        H.script ! A.src "/codemirror-3.x/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-3.x/mode/elm/elm.js" $ mempty
        mapM_ (\theme -> H.link ! A.rel "stylesheet" ! A.href (toValue ("/codemirror-3.x/theme/" ++ theme ++ ".css" :: String))) themes
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/misc/editor.css"
        H.script ! A.type_ "text/javascript" ! A.src "/misc/showdown.js" $ mempty
        H.script ! A.type_ "text/javascript" ! A.src "/misc/editor.js" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "editor_box" ! A.style "position:absolute;top:0;left:0;right:0;bottom:36px;" $ do
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.id "doc_desc" $ ""
           H.div ! A.id "doc_type" $ ""
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
               H.input ! A.class_ "valign" ! A.id "autocompile_checkbox" ! A.type_ "checkbox" !
                  A.onchange "toggleAutoCompile(this.checked)"
             H.div ! A.style "float:left; padding:6px;" $ do
               H.input ! A.class_ "valign" ! A.title "Ctrl+K: open doc in editor\nCtrl+Shift+K: open window/tab with doc" !
                  A.id "help_button" ! A.type_ "button" ! A.style "margin: 0 10px 0 0;" !
                  A.onclick "toggleDocView();" ! A.value "?"
               H.span ! A.title "Show the basic examples" $ do
                  H.span ! A.class_ "valign" $ "Examples:"
                  H.input ! A.class_ "valign" ! A.id "examples_checkbox" ! A.type_ "checkbox" !
                     A.onchange "toggleExamples(this.checked);"
               H.span ! A.style "padding-left: 16px;" ! A.class_ "valign" $
                    "Options:"
               H.input ! A.class_ "valign" ! A.id "options_checkbox" ! A.type_ "checkbox" !
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
               H.span ! A.style "padding-left: 16px;" ! A.class_ "valign" $
                    "Show type:"
               H.input ! A.class_ "valign" ! A.id "show_type_checkbox" ! A.type_ "checkbox" !
                  A.onchange "toggleShowType(this.checked);"
        H.script ! A.type_ "text/javascript" $ "initEditor();"
