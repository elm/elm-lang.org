import Html exposing (..)
import Html.Attributes exposing (..)

import Skeleton
import Center


main =
  Skeleton.docs
    "From JavaScript?"
    [ Center.markdown "800px" content
    , syntaxTable "Literals" literals
    , syntaxTable "Objects / Records" records
    , syntaxTable "Functions" functions
    , syntaxTable "Control Flow" controlFlow
    , syntaxTable "Strings" strings
    ]


content = """

The following tables show side-by-side mappings between JavaScript and Elm. A
lot of things are very similar, especially once you get used to the relatively
minor syntactic difference.

"""


-- TABLE


syntaxTable : String -> List (Value, Value) -> Html msg
syntaxTable subtitle comparisions =
  div
    (Center.styles "800px")
    [ h2 [] [text subtitle]
    , div [class "comparison"]
        [ table []
            [ tbody [] (header :: List.map row comparisions)
            ]
        ]
    , br [] []
    ]


header : Html msg
header =
  tr []
    [ th cellStyles [text "JavaScript"]
    , th cellStyles [text "Elm"]
    ]


row : (Value, Value) -> Html msg
row (js, elm) =
  tr []
    [ td cellStyles [value js]
    , td cellStyles [value elm]
    ]


cellStyles : List (Attribute msg)
cellStyles =
  [ style "width" "400px"
  , style "padding" "6px"
  ]


type Value = Code String | Message String


value v =
  case v of
    Code str ->
      code [] [text str]

    Message str ->
      span [ style "color" "#CBCBCB" ] [text str]


-- COMPARISONS

literals =
  [ (Code "3", Code "3")
  , (Code "3.1415", Code "3.1415")
  , (Code "\"Hello world!\"", Code "\"Hello world!\"")
  , (Code "'Hello world!'", Message "Cannot use single quotes for strings")
  , (Code "`multiline string`", Code "\"\"\"multiline string\"\"\"")
  , (Message "No distinction between characters and strings", Code "'a'")
  , (Code "true", Code "True")
  , (Code "[1,2,3]", Code "[1,2,3]")
  ]


records =
  [ (Code "{ x: 3, y: 4 }", Code "{ x = 3, y = 4 }")
  , (Code "point.x", Code "point.x")
  , (Code "point.x = 42", Code "{ point | x = 42 }")
  ]


functions =
  [ (Code "function(x, y) { return x + y; }", Code "\\x y -> x + y")
  , (Code "Math.max(3, 4)", Code "max 3 4")
  , (Code "Math.min(1, Math.pow(2, 4))", Code "min 1 (2^4)")
  , (Code "numbers.map(Math.sqrt)", Code "List.map sqrt numbers")
  , (Code "points.map(function(p) { return p.x })", Code "List.map .x points")
  ]


controlFlow =
  [ (Code "3 > 2 ? 'cat' : 'dog'", Code "if 3 > 2 then \"cat\" else \"dog\"")
  , (Code "var x = 42; ...", Code "let x = 42 in ...")
  , (Code "return 42", Message "Everything is an expression, no need for return")
  ]


strings =
  [ (Code "'abc' + '123'", Code "\"abc\" ++ \"123\"")
  , (Code "'abc'.length", Code "String.length \"abc\"")
  , (Code "'abc'.toUpperCase()", Code "String.toUpper \"abc\"")
  , (Code "'abc' + 123", Code "\"abc\" ++ toString 123")
  ]

