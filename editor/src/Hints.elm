port module Hints exposing (main)


import Browser
import Dict exposing (Dict)
import Docs
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Http
import Set exposing (Set)



-- PORTS


port submissions : (String -> msg) -> Sub msg
port cursorMoves : (Maybe String -> msg) -> Sub msg
port importEndLines : Int -> Cmd msg



-- MAIN


main : Program String Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { imports : ()
  , token : Maybe String
  , docs : Docs
  }


type Docs
  = Loading
  | Failure
  | Success Docs.Docs



-- INIT


init : String -> ( Model, Cmd Msg )
init source =
  (
    Model () Nothing Loading
  ,
    Http.get
      { url = "https://worker.elm-lang.org/compile/modules.json"
      , expect = Http.expectJson GotDocs Docs.decoder
      }
  )



-- UPDATE


type Msg
  = Submitted String
  | CursorMoved (Maybe String)
  | GotDocs (Result Http.Error Docs.Docs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Submitted source ->
      ( model
      , importEndLines 0
      )

    CursorMoved token ->
      ( { model | token = token }
      , Cmd.none
      )

    GotDocs result ->
      case result of
        Err _ ->
          ( { model | docs = Failure }
          , Cmd.none
          )

        Ok docs ->
          ( { model | docs = Success docs }
          , Cmd.none
          )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ submissions Submitted
    , cursorMoves CursorMoved
    ]



-- VIEW


view : Model -> Html msg
view model =
  case model.token of
    Nothing ->
      viewExamplesLink

    Just token ->
      lazy viewHint token



-- VIEW EXAMPLES LINK


viewExamplesLink : Html msg
viewExamplesLink =
  div [ class "hint" ]
    [ text "More Examples "
    , a [ href "/examples", target "_blank" ] [ text "Here" ]
    ]



-- VIEW HINT


viewHint : String -> Html msg
viewHint token =
  case Dict.get token keywords of
    Just link ->
      div [ class "hint" ]
        [ text "Hint: "
        , a [ href link.href, target "_blank" ] [ text link.text ]
        ]

    Nothing ->
      -- TODO lookup tokens in Dict of imported values
      -- TODO also handle "module:List" style tokens
      viewExamplesLink



-- KEYWORDS


-- NOTE: The logic in editor.js normalizes certain keywords:
--
--   1. "type" is converted to "alias" if it appears to be in a type alias
--   2. "as" is converted to "import" if it appears to be in an import
--
keywords : Dict String Link
keywords =
  Dict.fromList
    [ ("if", ifLink)
    , ("then", ifLink)
    , ("else", ifLink)
    -- TODO, ("let", letLink)
    -- TODO, ("in", letLink)
    , ("case", caseLink)
    , ("of", caseLink)
    , ("import", importLink)
    , ("exposing", importLink)
    -- TODO, ("as", Link "As Patterns" "/TODO")
    , (":", Link "Reading Types" "https://guide.elm-lang.org/types/reading_types.html")
    , ("type", Link "Custom Types" "https://guide.elm-lang.org/types/custom_types.html")
    , ("alias", Link "Type Aliases" "https://guide.elm-lang.org/types/type_aliases.html")
    , ("port", Link "Ports" "https://guide.elm-lang.org/interop/ports.html")
    ]



-- LINKS


type alias Link =
  { text : String
  , href : String
  }


ifLink : Link
ifLink =
  Link "Conditionals" "https://guide.elm-lang.org/core_language.html#if-expressions"


letLink : Link
letLink =
  Link "Let Expressions" "/TODO"


caseLink : Link
caseLink =
  Link "Pattern Matching" "https://guide.elm-lang.org/types/pattern_matching.html"


importLink : Link
importLink =
  Link "Imports" "https://github.com/elm/compiler/blob/master/hints/imports.md"
