import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import TopBar


main =
  div []
    [ TopBar.topBar "examples"
    , Center.markdown "600px" content
    , div [ Center.style "600px" ]
        [ div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
            [ view "Core" core
            , view "HTML" html
            , view "2D Graphics" visuals
            , view "Signals" signals
            ]
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

view : String -> List Section -> Html
view title sections =
  div [ class "examples", style ["width" => "300px"] ]
    [ h3 [] [text title]
    , ul [] (List.map viewSection sections)
    ]


viewSection : Section -> Html
viewSection (title, examples) =
  li []
    [ text title
    , ul [] (List.map viewExample examples)
    ]


viewExample : (String, String) -> Html
viewExample (name, url) =
  li [] [ a [href ("/examples/" ++ url)] [text name] ]


-- EXAMPLES

type alias Section = (String, List (String, String))


visuals : List Section
visuals =
  [ "collage" =>
      [ "lines" => "lines"
      , "shapes" => "shapes"
      , "text" => "collage-text"
      , "elements" => "collage-element"
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
      [ "use them" => "functions"
      , "infixes" => "infix"
      , "use fewer parens" => "forward-apply"
      , "define your own" => "define-functions"
      ]
  , "recursion" =>
      [ "list length" => "length"
      , "zip" => "zip"
      , "quick sort" => "quick-sort"
      , "merge sort" => "merge-sort"
      ]
  , "union types" =>
      [ "either" => "either"
      , "binary tree" => "binary-tree"
      , "boolean expressions" => "boolean-expressions"
      ]
  ]


html : List Section
html =
  [ "basics" =>
      [ "hello world!" => "hello-html"
      , "unordered list" => "unordered-list"
      , "markdown" => "markdown"
      ]
  , "user input" =>
      [ "buttons" => "buttons"
      , "field" => "field"
      , "password" => "password"
      , "checkboxes" => "checkboxes"
      , "radio buttons" => "radio-buttons"
      ]
  , "larger examples" =>
      [ "calculator" => "calculator"
      ]
  ]


signals : List Section
signals =
  [ "mouse" =>
      [ "position" => "mouse-position"
      , "is down" => "mouse-is-down"
      , "clicks" => "mouse-clicks"
      , "yogi" => "resize-yogi"
      , "tracker" => "mouse-tracker"
      ]
  , "window" =>
      [ "size" => "resize-paint"
      , "centering" => "centering"
      ]
  , "keyboard" =>
      [ "arrows" => "arrows"
      , "wasd" => "wasd"
      , "keys down" => "keys"
      , "key presses" => "key-presses"
      ]
  , "time" =>
      [ "clock" => "clock"
      ]
  ]