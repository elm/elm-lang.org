{-# LANGUAGE OverloadedStrings #-}
module Init.Examples (init) where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Maybe as Maybe
import Data.Monoid (mempty)
import Prelude hiding (init)
import System.Directory (copyFile, getDirectoryContents)
import System.FilePath ((</>), (<.>), splitExtension)
import Text.Blaze.Html (Html, (!), toHtml, preEscapedToMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Init.FileTree as FT
import Init.Helpers (make, write)


init :: IO ()
init =
  do  write "Setting up examples ."
      make
        ("src" </> "editor" </> "EditorControls" <.> "elm")
        (FT.file ["editor"] "controls" "js")
      copyFile
        ("src" </> "editor" </> "editor" <.> "css")
        (FT.file ["editor"] "editor" "css")
      copyFile
        ("src" </> "editor" </> "editor" <.> "js")
        (FT.file ["editor"] "editor" "js")

      examples <- collectExamples
      mapM_ initExample examples
      initTry

      putStrLn " done\n"


initExample :: String -> IO ()
initExample name =
  do  write "."
      initCode name
      initResult name
      initEditor name


initTry :: IO ()
initTry =
  do  BS.writeFile
          (FT.file ["examples","code"] "try" "html")
          (Blaze.renderHtml (code "try" ""))

      let html = editor "Try Elm" "examples/try/code" "try-message"

      BS.writeFile
          (FT.file ["examples","editor"] "try" "html")
          (Blaze.renderHtml html)


-- COLLECT ALL EXAMPLES

collectExamples :: IO [String]
collectExamples =
  do  contents <- getDirectoryContents ("src" </> "examples")
      return (Maybe.mapMaybe isElm contents)


isElm :: String -> Maybe String
isElm fileName =
  case splitExtension fileName of
    (name, ".elm") -> Just name
    _ -> Nothing


-- INIT EDITOR FILES

initEditor :: String -> IO ()
initEditor name =
  do  let html =
            editor
              ("example/" ++ name)
              ("examples/" ++ name ++ "/code")
              ("examples/" ++ name ++ "/result")

      BS.writeFile
          (FT.file ["examples","editor"] name "html")
          (Blaze.renderHtml html)


editor :: String -> FilePath -> FilePath -> Html
editor title codePath resultPath =
  H.docTypeHtml $ do
    H.head . H.title $ toHtml title
    preEscapedToMarkup $
        concat
          [ "<frameset cols=\"50%,50%\">\n"
          , "  <frame name=\"input\" src=\"/", codePath, "\" />\n"
          , "  <frame name=\"output\" src=\"/", resultPath, "\" />\n"
          , "</frameset>"
          ]


-- INIT CODE FILES

initCode :: String -> IO ()
initCode name =
  do  source <- readFile ("src" </> "examples" </> name <.> "elm")
      BS.writeFile
          (FT.file ["examples","code"] name "html")
          (Blaze.renderHtml (code name source))


code :: String -> String -> Html
code name source =
    H.docTypeHtml $
    H.html $ do
      H.head $ do
        H.title . toHtml $ name
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/lib/codemirror.css"
        H.script ! A.src "/codemirror-5.0/lib/codemirror.js" $ mempty
        H.script ! A.src "/codemirror-5.0/mode/elm/elm.js" $ mempty
        H.script ! A.src "/editor/controls.js" $ mempty
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/theme/elegant.css"
        H.link ! A.rel "stylesheet" ! A.href "/codemirror-5.0/theme/mbo.css"
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/editor/editor.css"
        H.script ! A.type_ "text/javascript" ! A.src "/editor/editor.js" $ mempty
      H.body $ do
        H.form ! A.id "inputForm" ! A.action "/compile" ! A.method "post" ! A.target "output" $ do
           H.div ! A.id "controls" $ ""
           H.div ! A.id "editor_box" $
             H.textarea ! A.name "input" ! A.id "input" $ toHtml source
        H.script ! A.type_ "text/javascript" $ "initEditor();"


-- INIT RESULT FILES

initResult :: String -> IO ()
initResult name =
  make
    ("src" </> "examples" </> name <.> "elm")
    (FT.file ["examples","result"] name "html")
