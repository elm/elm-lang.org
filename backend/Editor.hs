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


-- | Create an HTML document that allows you to edit and submit Elm code
--   for compilation.
editor :: FilePath -> String -> Html
editor filePath code =
    H.docTypeHtml $
    H.html $ do
      H.head $ do
        H.title . toHtml $ "Elm Editor: " ++ FP.takeBaseName filePath
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/lib/codemirror.css"
        H.script ! A.src "/codemirror-5.0/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-5.0/mode/elm/elm.js" $ mempty
        H.script ! A.src "/editor-controls.js" $ mempty
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/theme/elegant.css"
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/theme/mbo.css"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/misc/editor.css"
        H.script ! A.type_ "text/javascript" ! A.src "/misc/editor.js" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "controls" $ ""
           H.div ! A.id "editor_box" $
             H.textarea ! A.name "input" ! A.id "input" $ toHtml code
        H.script ! A.type_ "text/javascript" $ "initEditor();"
