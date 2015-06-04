import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import TopBar


main =
  div []
    [ TopBar.topBar "docs"
    , Center.markdown "600px" content
    , syntaxTable "Literals" literals
    , syntaxTable "Objects / Records" records
    , syntaxTable "Functions" functions
    , syntaxTable "Control Flow" controlFlow
    , syntaxTable "Strings" strings
    ]


content = """

# From JavaScript?

The following tables show side-by-side mappings between JavaScript on the left
and Elm on the right. A lot of things are very similar, especially once you get
used to the relatively minor syntactic difference.

"""


-- TABLE

syntaxTable subtitle comparisions =
  div [Center.style "800px"]
    [ h2 [] [text subtitle]
    , div [class "comparison"]
        [ tbody []
            [ table [] (List.map row comparisions)
            ]
        ]
    , br [] []
    ]


row (js, elm) =
  tr []
    [ td [cellStyle] [value js]
    , td [cellStyle] [value elm]
    ]


cellStyle =
  style [ ("width", "400px"), ("padding", "6px") ]


value v =
  case v of
    Code str ->
      code [] [text str]

    Message str ->
      span [style [("color", "#CBCBCB")]] [text str]


-- COMPARISONS

type Value = Code String | Message String


vs js elm =
  (Code js, Code elm)


literals =
  [ "3" `vs` "3"
  , "3.1415" `vs` "3.1415"
  , "\"Hello world!\"" `vs` "\"Hello world!\""
  , (Message "Multiline strings not widely supported", Code "\"\"\"multiline string\"\"\"")
  , (Code "'Hello world!'", Message "Cannot use single quotes for strings")
  , (Message "No distinction between characters and strings", Code "'a'")
  , "true" `vs` "True"
  , "[1,2,3]" `vs` "[1,2,3]"
  ]


records =
  [ "{ x: 3, y: 4 }" `vs` "{ x = 3, y = 4 }"
  , "point.x" `vs` "point.x"
  , "point.x = 42" `vs` "{ point | x <- 42 }"
  ]


functions =
  [ "function(x,y) { return x + y; }" `vs` "\\x y -> x + y"
  , "Math.max(3, 4)" `vs` "max 3 4"
  , "Math.min(1, Math.pow(2, 4))" `vs` "min 1 (2^4)"
  , "numbers.map(Math.sqrt)" `vs` "List.map sqrt numbers"
  , "points.map(function(p) { return p.x })" `vs` "List.map .x points"
  ]


controlFlow =
  [ "3 > 2 ? 'cat' : 'dog'" `vs` "if 3 > 2 then \"cat\" else \"dog\""
  , "var x = 42; ..." `vs` "let x = 42 in ..."
  ]


strings =
  [ "'abc' + '123'" `vs` "\"abc\" ++ \"123\""
  , "'abc'.length" `vs` "String.length \"abc\""
  , "'abc'.toUpperCase()" `vs` "String.toUpper \"abc\""
  ]

