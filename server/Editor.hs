{-# LANGUAGE OverloadedStrings #-}
module Editor (editor,ide,emptyIDE) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import qualified System.FilePath as FP

-- | Display an editor and the compiled result side-by-side.
ide :: FilePath -> String -> Html
ide fileName code = ideBuilder ("Elm Editor: " ++ FP.takeBaseName fileName)
                               fileName
                               ("/compile?input=" ++ urlEncode code)

-- | Display an editor and the compiled result side-by-side.
emptyIDE :: Html
emptyIDE = ideBuilder "Try Elm" "Empty.elm" "/Try.elm"

ideBuilder :: String -> String -> String -> Html
ideBuilder title input output =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ title
      preEscapedToMarkup $ 
         concat [ "<frameset cols=\"50%,50%\">\n"
                , "  <frame name=\"input\" src=\"/code/", input, "\" />\n"
                , "  <frame name=\"output\" src=\"", output, "\" />\n"
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
        H.title . toHtml $ "Elm Editor: " ++ FP.takeBaseName filePath
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-3.x/lib/codemirror.css"
        H.script ! A.src "/codemirror-3.x/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-3.x/mode/elm/elm.js" $ mempty
        mapM_ (\theme -> H.link ! A.rel "stylesheet" ! A.href (toValue ("/codemirror-3.x/theme/" ++ theme ++ ".css" :: String))) themes
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/misc/editor.css"
        H.script ! A.type_ "text/javascript" ! A.src "/misc/showdown.js" $ mempty
        H.script ! A.type_ "text/javascript" ! A.src "/misc/editor.js?v0.9" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "editor_box" $ do
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.id "options" $ do
             bar "documentation" docs
             bar "editor_options" editorOptions
             bar "always_on" (buttons >> options)
        H.script ! A.type_ "text/javascript" $ "initEditor();"

bar :: AttributeValue -> Html -> Html
bar id body = H.div ! A.id id ! A.class_ "option" $ body

buttons :: Html
buttons = H.div ! A.class_ "valign_kids"
                ! A.style "float:right; padding-right: 6px;"
                $ "Auto-Swap" >> autoBox >> "     " >> hotSwapButton >> compileButton
      where
        hotSwapButton = 
            H.input
                 ! A.type_ "button"
                 ! A.id "hot_swap_button"
                 ! A.value "Hot Swap"
                 ! A.onclick "hotSwap()"
                 ! A.title "Ctrl-Shift-Enter"

        compileButton = 
            H.input
                 ! A.type_ "button"
                 ! A.id "compile_button"
                 ! A.value "Compile"
                 ! A.onclick "compile()"
                 ! A.title "Ctrl-Enter: change program behavior but keep the state"

        autoBox =
            H.input
                 ! A.type_ "checkbox"
                 ! A.id "auto_hot_swap_checkbox"
                 ! A.onchange "setAutoHotSwap(this.checked)"
                 ! A.title "attempt to hot-swap automatically"


options :: Html
options = H.div ! A.class_ "valign_kids"
                ! A.style "float:left; padding-left:6px;"
                $ (docs >> opts)
    where 
      docs =
          H.input
               ! A.type_ "button"
               ! A.id "help_button"
               ! A.value "?"
               ! A.style "margin-right: 10px;"
               ! A.onclick "toggleVerbose();"
               ! A.title "Ctrl+K: open doc in editor\nCtrl+Shift+K: open window/tab with doc"
      opts = do
        H.span $ "Options:"
        H.input ! A.type_ "checkbox"
                ! A.id "options_checkbox" 
                ! A.onchange "showOptions(this.checked);"

editorOptions :: Html
editorOptions = theme >> zoom >> lineNumbers
    where
      optionFor :: String -> Html
      optionFor text =
          H.option ! A.value (toValue text) $ toHtml text

      theme =
          H.select ! A.id "editor_theme"
                   ! A.onchange "setTheme(this.value)"
                   $ mapM_ optionFor themes
              
      zoom =
          H.select ! A.id "editor_zoom"
                   ! A.onchange "setZoom(this.options[this.selectedIndex].innerHTML)"
                   $ mapM_ optionFor ["100%", "80%", "150%", "200%"]

      lineNumbers = do
        H.span ! A.style "padding-left: 16px;" $ "Line Numbers:"
        H.input ! A.type_ "checkbox"
                ! A.id "editor_lines"
                ! A.onchange "showLines(this.checked);"
        H.span ! A.style "padding-left: 16px;" $ "Show type:"
        H.input ! A.type_ "checkbox"
                ! A.id "show_type_checkbox"
                ! A.onchange "showType(this.checked);"

docs :: Html
docs = tipe >> desc
    where
      tipe = H.div ! A.class_ "type" $ ""
      desc = H.div ! A.class_ "doc"
                   ! A.style "display:none;"
                   $ ""