import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import TopBar


main =
  div []
    [ TopBar.topBar "examples"
    , Center.markdown "600px" content
    , div [ Center.style "600px" ]
        [ h1 [] [text "Examples"]
        , ul [] (List.map viewSection core)
        , ul [] (List.map viewSection visuals)
        , ul [] (List.map viewSection signals)
        ]
    ]


(=>) = (,)


content = """

# Learn by Example

Walk through a sequence of small examples, building skills one at a time by
reading and modifying Elm code in the [online editor](/try).

Remember to check the [syntax reference](/docs/syntax) and [docs](/docs) when
you see new syntax or features!

"""


-- VIEW EXAMPLES

viewSection : Section -> Html
viewSection (title, examples) =
  li []
    [ text title
    , ul [] (List.map viewExample examples)
    ]


viewExample : (String, String) -> Html
viewExample (name, url) =
  li [] [ a [href url] [text name] ]


-- EXAMPLES

type alias Section = (String, List (String, String))


visuals : List Section
visuals =
  [ "collage" =>
      [ "lines" => "lines"
      , "shapes" => "shapes"
      , "text" => "text"
      , "elements" => "elements"
      , "transforms" => "transforms"
      , "color" => "color"
      , "linear gradient" => "linear-gradient"
      , "radial gradient" => "radial-gradient"
      , "texture" => "texture"
      ]
  ]


core : List Section
core =
  [ "functions" =>
      [ "apply" => "apply"
      , "forward apply" => "forward-apply"
      , "infixes" => "infix"
      , "composition" => "composition"
      , "anonymous" => "anonymous"
      ]
  , "recursion" =>
      [ "factorial" => "factorial"
      , "list length" => "length"
      , "zip" => "zip"
      , "quick sort" => "quick-sort"
      ]
  , "union types" =>
      [ "maybe" => "maybe"
      , "binary tree" => "binary-tree"
      , "boolean expressions" => "boolean-expressions"
      ]
  , "containers" =>
      [ "list" => "list"
      , "dict" => "dict"
      , "set" => "set"
      ]
  ]


signals : List Section
signals =
  [ "mouse" =>
      [ "position" => "position"
      , "presses" => "isDown"
      , "clicks" => "countClicks"
      , "yogi" => "resizeYogi"
      , "track" => "transforms"
      ]
  , "keyboard" =>
      [ "arrows" => "arrows"
      , "wasd" => "wasd"
      , "keys Down" => "keysDown"
      , "key Presses" => "charPressed"
      ]
  , "touch" =>
      [ "raw" => "touches"
      , "touches" => "touch"
      , "taps" => "taps"
      , "draw" => "draw"
      ]
  , "window" =>
      [ "size" => "resizePaint"
      , "centering" => "centering"
      ]
  , "time" =>
      [ "fPS" => "fps"
      , "every" => "every"
      ]
  ]