import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import TopBar


port title : String
port title =
  "Elm Examples"


main =
  div []
    [ TopBar.topBar "examples"
    , Center.markdown "600px" content
    , div [ Center.style "600px" ]
        [ view "Core" core
        , view "HTML" html
        , view "Visuals" visuals
        , view "Signals" signals
        , view "Games" games
        ]
    ]


content = """

# Learn by Example

Walk through a sequence of small examples, building skills one at a time by
reading and modifying Elm code in the [online editor](/try).

Remember to check the [syntax reference](/docs/syntax) and [docs](/docs) when
you see new syntax or features!

"""


-- VIEW EXAMPLES

(=>) = (,)


view : String -> List Section -> Html
view title sections =
  div
    [ class "examples"
    , style ["width" => "300px", "display" => "inline-block", "vertical-align" => "top"]
    ]
    [ h3 [] [text title]
    , ul [] (List.map viewSection sections)
    ]


viewSection : Section -> Html
viewSection (title, examples) =
  li []
    [ text title
    , ul [] (List.map viewExample examples)
    ]


viewExample : (String, Example) -> Html
viewExample (name, example) =
  let
    url =
      case example of
        Local fileName ->
            "/examples/" ++ fileName
        Foreign foreignUrl ->
            foreignUrl
  in
    li [] [ a [href url] [text name] ]


-- EXAMPLES

type alias Section = (String, List (String, Example))

type Example
  = Local String
  | Foreign String


(==>) name fileName =
  (name, Local fileName)


visuals : List Section
visuals =
  [ "2D graphics" =>
      [ "lines" ==> "lines"
      , "shapes" ==> "shapes"
      , "text" ==> "collage-text"
      , "elements" ==> "collage-element"
      , "transforms" ==> "transforms"
      , "color" ==> "color"
      , "linear gradient" ==> "linear-gradient"
      , "radial gradient" ==> "radial-gradient"
      , "texture" ==> "texture"
      ]
  , "layout" =>
      [ "hello world" ==> "hello-element"
      , "simple layout" ==> "layout-simple"
      , "fancier layout" ==> "layout-fancy"
      , "centering" ==> "centering"
      ]
  , "3D graphics" =>
      [ "triangle" ==> "triangle"
      , "cube" ==> "cube"
      , "crate" ==> "crate"
      , "thwomp" ==> "thwomp"
      , "first person" ==> "first-person"
      ]
  ]


core : List Section
core =
  [ "functions" =>
      [ "use them" ==> "functions"
      , "infixes" ==> "infix"
      , "use fewer parens" ==> "forward-apply"
      , "define your own" ==> "define-functions"
      ]
  , "recursion" =>
      [ "list length" ==> "length"
      , "zip" ==> "zip"
      , "quick sort" ==> "quick-sort"
      , "merge sort" ==> "merge-sort"
      ]
  , "union types" =>
      [ "either" ==> "either"
      , "binary tree" ==> "binary-tree"
      , "boolean expressions" ==> "boolean-expressions"
      ]
  ]


html : List Section
html =
  [ "basics" =>
      [ "hello world!" ==> "hello-html"
      , "unordered list" ==> "unordered-list"
      , "markdown" ==> "markdown"
      ]
  , "user input" =>
      [ "buttons" ==> "buttons"
      , "field" ==> "field"
      , "password" ==> "password"
      , "checkboxes" ==> "checkboxes"
      , "radio buttons" ==> "radio-buttons"
      ]
  , "larger examples" =>
      [ "dynamic list" => Foreign "https://github.com/evancz/elm-architecture-tutorial/"
      , "todo list" => Foreign "https://github.com/evancz/elm-todomvc"
      ]
  ]


signals : List Section
signals =
  [ "mouse" =>
      [ "position" ==> "mouse-position"
      , "is down" ==> "mouse-is-down"
      , "clicks" ==> "mouse-clicks"
      , "yogi" ==> "resize-yogi"
      , "tracker" ==> "mouse-tracker"
      , "stamps" ==> "stamps"
      ]
  , "window" =>
      [ "size" ==> "resize-paint"
      , "centering" ==> "window-centering"
      ]
  , "keyboard" =>
      [ "arrows" ==> "arrows"
      , "wasd" ==> "wasd"
      , "keys down" ==> "keys"
      , "key presses" ==> "key-presses"
      ]
  , "time" =>
      [ "clock" ==> "clock"
      ]
  ]


games : List Section
games =
  [ "simple" =>
      [ "short mario" ==> "short-mario"
      , "idiomatic mario" ==> "idiomatic-mario"
      , "adventure" ==> "adventure"
      , "pong" ==> "pong"
      ]
  , "community" =>
      [ "Tetris" => Foreign "https://github.com/jcollard/elmtris"
      , "Breakout" => Foreign "https://github.com/Dobiasd/Breakout#breakout--play-it"
      , "Maze" => Foreign "https://github.com/Dobiasd/Maze#maze--play-it"
      , "Concentration" => Foreign "https://github.com/Dobiasd/Demoscene-Concentration"
      , "Froggy" => Foreign "https://github.com/thSoft/froggy"
      ]
  ]