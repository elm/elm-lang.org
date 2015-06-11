module Init.Guide (init, chapters) where

import Control.Monad (when)
import qualified Data.Maybe as Maybe
import Prelude hiding (init)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))

import qualified Init.FileTree as FT
import Init.Helpers (makeWithStyle, write, isOutdated)


-- CHAPTERS

chapters :: [String]
chapters =
  [ "core-language"
  , "model-the-problem"
  , "architecture"
  , "reactivity"
  , "interop"
  ]


-- INITIALIZE

init :: IO ()
init =
  do  write "Setting up guide ."

      outline <- mapM initChapter chapters
      writeFile (FT.file ["guide","elm"] "Outline" "elm") (toOutline outline)
      mapM generateHtml chapters

      putStrLn " done\n"


initChapter :: String -> IO (String, [String])
initChapter name =
  do  let input = "src" </> "guide" </> "chapters" </> name <.> "md"
      markdown <- readFile input
      let mdLines = lines markdown
      case Maybe.mapMaybe toTitle mdLines of
        [] ->
            do  putStrLn $ " no title found for '" ++ name ++ "'!\n"
                exitFailure

        [title] ->
            do  let output = FT.file ["guide","elm"] name "elm"
                outdated <- isOutdated input output
                let content = unlines (filter notTitle mdLines)
                when outdated (writeFile output (toElm title content))
                return (title, Maybe.mapMaybe toSubtitle mdLines)

        _ ->
            do  putStrLn $ " fould multiple titles for '" ++ name ++ "'!\n"
                exitFailure


generateHtml :: String -> IO Bool
generateHtml name =
  do  write "."
      makeWithStyle
        (FT.file ["guide", "elm"] name "elm")
        (FT.file ["guide", "html"] name "html")


-- CONVERSIONS

toTitle :: String -> Maybe String
toTitle line =
  case line of
    '#' : ' ' : title ->
        Just title
    _ ->
        Nothing


notTitle :: String -> Bool
notTitle line =
  Maybe.isNothing (toTitle line)


toSubtitle :: String -> Maybe String
toSubtitle line =
  case line of
    '#' : '#' : ' ' : subtitle ->
        Just subtitle
    _ ->
        Nothing


toElm :: String -> String -> String
toElm title markdown =
  unlines
    [ "import Html exposing (..)"
    , "import Html.Attributes exposing (..)"
    , "import Blog"
    , "import Center"
    , "import Outline"
    , "import TopBar"
    , ""
    , "port title : String"
    , "port title ="
    , "  " ++ show title
    , ""
    , "main ="
    , "  Blog.docs " ++ show title ++ " [ Center.markdown \"600px\" info ]"
    , ""
    , "info = \"\"\"\n" ++ markdown ++ "\n\"\"\""
    ]


toOutline :: [(String, [String])] -> String
toOutline outline =
  unlines
    [ "module Outline where"
    , ""
    , "outline ="
    , concat (zipWith (++) ("  [ " : repeat "\n  , ") (map show outline))
    , "  ]"
    ]