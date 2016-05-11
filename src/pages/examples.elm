import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton
    "examples"
    [ Center.markdown "600px" content
    , div [ Center.style "600px" ]
        [ view "HTML" html
        , view "Functional Stuff" core
        , view "Effects" effects
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


view : String -> List Section -> Html msg
view title sections =
  div
    [ class "examples"
    , style ["width" => "300px", "display" => "inline-block", "vertical-align" => "top"]
    ]
    [ h3 [] [text title]
    , ul [] (List.map viewSection sections)
    ]


viewSection : Section -> Html msg
viewSection (title, examples) =
  li []
    [ text title
    , ul [] (List.map viewExample examples)
    ]


viewExample : (String, Example) -> Html msg
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
      , "form" ==> "form"
      , "checkboxes" ==> "checkboxes"
      , "radio buttons" ==> "radio-buttons"
      ]
  , "larger examples" =>
      [ "todo list" => Foreign "https://github.com/evancz/elm-todomvc"
      ]
  ]


effects : List Section
effects =
  [ "commands" =>
      [ "random" ==> "random"
      , "http" ==> "http"
      ]
  , "subscriptions" =>
      [ "time" ==> "time"
      , "websockets" ==> "websockets"
      , "mouse drags" ==> "drag"
      ]
  ]

