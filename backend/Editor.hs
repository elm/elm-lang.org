{-# LANGUAGE OverloadedStrings #-}
module Editor ( editor, ide, empty ) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified System.FilePath as FP


-- | Display an editor and the compiled result side-by-side.
empty :: String -> Html
empty cols =
    ideHtml cols "Try Elm" "empty" "Try.elm"


-- | Display an editor and the compiled result side-by-side.
ide :: String -> FilePath -> Html
ide cols filePath =
    ideHtml
      cols
      ("Elm Edit: " ++ FP.dropExtension (FP.takeBaseName filePath))
      filePath
      filePath


ideHtml :: String -> String -> FilePath -> FilePath -> Html
ideHtml cols title codePath resultPath =
    H.docTypeHtml $ do
        H.head . H.title $ toHtml title
        preEscapedToMarkup $ 
            concat
              [ "<frameset cols=\"" ++ cols ++ "\">\n"
              , "  <frame name=\"input\" src=\"/code/", codePath, "\" />\n"
              , "  <frame name=\"output\" src=\"/", resultPath, "\" />\n"
              , "</frameset>"
              ]


-- | list of themes to use with CodeMirror
themes :: [String]
themes =
  [ "ambiance", "blackboard", "cobalt", "eclipse"
  , "elegant", "erlang-dark", "lesser-dark", "monokai", "neat", "night"
  , "rubyblue", "solarized", "twilight", "vibrant-ink", "xq-dark"
  ]


-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: FilePath -> String -> Html
editor filePath code =
    H.docTypeHtml $
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ FP.takeBaseName filePath
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-3.x/lib/codemirror.css"
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-3.x/lib/util/foldgutter.css"
        H.script ! A.src "/codemirror-3.x/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-3.x/lib/util/foldcode.js" $ mempty
        H.script ! A.src "/codemirror-3.x/lib/util/foldgutter.js" $ mempty
        H.script ! A.src "/codemirror-3.x/lib/util/indent-fold.js" $ mempty
        H.script ! A.src "/codemirror-3.x/lib/util/brace-fold.js" $ mempty
        H.script ! A.src "/codemirror-3.x/lib/util/comment-fold.js" $ mempty
        H.script ! A.src "/codemirror-3.x/mode/elm/elm.js" $ mempty
        mapM_ (\theme -> H.link ! A.rel "stylesheet" ! A.href (toValue ("/codemirror-3.x/theme/" ++ theme ++ ".css" :: String))) themes
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/misc/editor.css"
        H.script ! A.type_ "text/javascript" ! A.src "/misc/showdown.js" $ mempty
        H.script ! A.type_ "text/javascript" ! A.src "/misc/editor.js?0.11" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "editor_box" $
             H.textarea ! A.name "input" ! A.id "input" $ toHtml ('\n':code)
           H.div ! A.id "options" $ do
             bar "documentation" docs
             bar "editor_options" editorOptions
             bar "always_on" (buttons >> options)
        H.script ! A.type_ "text/javascript" $ "initEditor();"


bar :: AttributeValue -> Html -> Html
bar id' body =
    H.div ! A.id id' ! A.class_ "option" $ body


buttons :: Html
buttons =
    H.div
      ! A.class_ "valign_kids"
      ! A.style "float:right; padding-right: 6px;"
      $ "Auto-update:" >> autoBox >> hotSwapButton >> compileButton
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
        ! A.style "margin-right:20px;"
        ! A.title "attempt to hot-swap automatically"


options :: Html
options =
    H.div
      ! A.class_ "valign_kids"
      ! A.style "float:left; padding-left:6px; padding-top:2px;"
      ! A.title "Show documentation and types."
      $ (docs' >> opts)
  where 
    docs' =
      do  H.span "Hints:"
          H.input
            ! A.type_ "checkbox"
            ! A.id "show_type_checkbox"
            ! A.onchange "showType(this.checked);"

    opts =
      do  H.span ! A.style "padding-left: 12px;" $ "Options:"
          H.input
            ! A.type_ "checkbox"
            ! A.id "options_checkbox" 
            ! A.onchange "showOptions(this.checked);"


editorOptions :: Html
editorOptions =
    theme >> zoom >> lineNumbers >> codeFolding
  where
    optionFor :: String -> Html
    optionFor text =
        H.option ! A.value (toValue text) $ toHtml text

    theme =
      H.select
        ! A.id "editor_theme"
        ! A.onchange "setTheme(this.value)"
        $ mapM_ optionFor themes
    
    zoom =
      H.select
        ! A.id "editor_zoom"
        ! A.onchange "setZoom(this.options[this.selectedIndex].innerHTML)"
        $ mapM_ optionFor ["100%", "80%", "150%", "200%"]

    lineNumbers = do
      H.span ! A.style "padding-left: 16px;" $ "Line Numbers:"
      H.input
        ! A.type_ "checkbox"
        ! A.id "editor_lines"
        ! A.onchange "showLines(this.checked);"

    codeFolding = do
      H.span ! A.style "padding-left: 16px;" $ "Code Folding:"
      H.input
        ! A.type_ "checkbox"
        ! A.id "editor_code_folding"
        ! A.onchange "showCodeFolding(this.checked);"

docs :: Html
docs =
    tipe >> desc
  where
    tipe =
      H.div ! A.class_ "type" $ message >> more

    message =
      H.div ! A.style "position:absolute; left:4px; right:36px; overflow:hidden; text-overflow:ellipsis;" $ ""

    more =
      H.a
        ! A.id "toggle_link"
        ! A.style "display:none; float:right;"
        ! A.href "javascript:toggleVerbose();"
        ! A.title "Ctrl+H"
        $ "more"

    desc =
      H.div
        ! A.class_ "doc"
        ! A.style "display:none;"
        $ ""

