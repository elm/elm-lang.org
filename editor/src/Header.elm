module Header exposing
  ( Imports(..)
  , Import
  , Exposing(..)
  , Exposed
  , parse
  , defaultImports
  )


import Dict exposing (Dict)
import Parser exposing (..)
import Set exposing (Set)



-- IMPORTS


type Imports =
  Imports (List Import)


defaultImports : Imports
defaultImports =
  Imports defaults



-- IMPORT


type alias Import =
  { module_ : String
  , alias_ : Maybe String
  , exposing_ : Exposing
  }


type Exposing
  = All
  | Some Exposed
  | None


type alias Exposed =
  { ops : Set String
  , lower : Set String
  , upper : Dict String Bool
  }



-- PARSE


parse : String -> Maybe (Imports, Int)
parse source =
  Result.toMaybe (Parser.run parser source)


parser : Parser (Imports, Int)
parser =
  succeed Tuple.pair
    |. whitespace
    |. newline
    |. oneOf
        [ module_
            |. whitespace
            |. newline
        , succeed ()
        ]
    |= imports
    |= getRow



-- MODULE


module_ : Parser ()
module_ =
  oneOf [ keyword "port" |. whitespace, succeed () ]
    |. keyword "module"
    |. whitespace
    |. moduleName
    |. whitespace
    |. exposing_



-- IMPORTS


imports : Parser Imports
imports =
  loop defaults importsHelp


importsHelp : List Import -> Parser (Step (List Import) Imports)
importsHelp revImports =
  oneOf
    [ map (\i -> Loop (i :: revImports)) import_
    , succeed (Done (Imports revImports))
    ]



-- IMPORT


import_ : Parser Import
import_ =
  succeed Import
    |. keyword "import"
    |. whitespace
    |= moduleName
    |. whitespace
    |= oneOf
        [ succeed Just
            |. keyword "as"
            |. whitespace
            |= upper
            |. whitespace
        , succeed Nothing
        ]
    |= oneOf
        [ exposing_
        , succeed None
        ]
    |. whitespace
    |. newline



-- EXPOSING


exposing_ : Parser Exposing
exposing_ =
  succeed identity
    |. keyword "exposing"
    |. whitespace
    |= oneOf
        [ backtrackable (exposingDotDot All)
        , map toExposing <|
            sequence
              { start = "("
              , separator = ","
              , end = ")"
              , spaces = whitespace
              , item = exposedItem
              , trailing = Forbidden
              }
        ]


toExposing : List ExposedItem -> Exposing
toExposing items =
  Some <| List.foldl addItem (Exposed Set.empty Set.empty Dict.empty) items


addItem : ExposedItem -> Exposed -> Exposed
addItem item exposed =
  case item of
    Op name         -> { exposed | ops = Set.insert name exposed.ops }
    Lower name      -> { exposed | lower = Set.insert name exposed.lower }
    Upper name bool -> { exposed | upper = Dict.insert name bool exposed.upper }



-- EXPOSED ITEM


type ExposedItem
  = Op String
  | Lower String
  | Upper String Bool


exposedItem : Parser ExposedItem
exposedItem =
  oneOf
    [ map Lower lower
    , map Op operator
    , succeed Upper
        |= upper
        |. whitespace
        |= oneOf
            [ exposingDotDot True
            , succeed False
            ]
    ]


exposingDotDot : a -> Parser a
exposingDotDot value =
  succeed value
    |. symbol "("
    |. whitespace
    |. symbol ".."
    |. whitespace
    |. symbol ")"



-- VARIABLES
--
-- Fairly loosey-goosey implementations here.
-- No checking for keywords and no checking for
-- valid module names. (e.g. Html....attrs) is
-- allowed.


lower : Parser String
lower =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.empty
    }


upper : Parser String
upper =
  variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.empty
    }


moduleName : Parser String
moduleName =
  variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_' || c == '.'
    , reserved = Set.empty
    }



-- WHITESPACE


newline : Parser ()
newline =
  getCol
    |> andThen (\col -> if col == 1 then succeed () else problem "")


whitespace : Parser ()
whitespace =
  loop 0 spaceStepper


spaceStepper : Int -> Parser (Step Int ())
spaceStepper oldOffset =
  succeed (\newOffset -> if oldOffset == newOffset then Done () else Loop newOffset)
    |. whitespaceChunk
    |= getOffset


whitespaceChunk : Parser ()
whitespaceChunk =
  oneOf
    [ lineComment "--"
    , multiComment "{-" "-}" Nestable
    , spaces
    ]



-- OPERATORS


operator : Parser String
operator =
  succeed identity
    |. symbol "("
    |= getChompedString (chompIf isOperatorChar |. chompWhile isOperatorChar)
    |. symbol ")"


isOperatorChar : Char -> Bool
isOperatorChar char =
  Set.member char operatorChars


operatorChars : Set.Set Char
operatorChars =
  Set.fromList (String.toList "+-/*=.<>:&|^?%!")



-- DEFAULTS


defaults : List Import
defaults =
  [ Import "Basics" Nothing All
  , Import "Debug" Nothing None
  , Import "List" Nothing listExposed
  , Import "Maybe" Nothing (typeOpen "Maybe")
  , Import "Result" Nothing (typeOpen "Result")
  , Import "String" Nothing (typeClosed "String")
  , Import "Char" Nothing (typeClosed "Char")
  , Import "Tuple" Nothing None
  , Import "Platform" Nothing (typeClosed "Program")
  , Import "Platform.Cmd" (Just "Cmd") (typeClosed "Cmd")
  , Import "Platform.Sub" (Just "Sub") (typeClosed "Sub")
  ]


typeOpen : String -> Exposing
typeOpen name =
  Some <| Exposed Set.empty Set.empty (Dict.singleton name True)


typeClosed : String -> Exposing
typeClosed name =
  Some <| Exposed Set.empty Set.empty (Dict.singleton name False)


listExposed : Exposing
listExposed =
  Some <| Exposed (Set.singleton "::") Set.empty (Dict.singleton "List" False)
