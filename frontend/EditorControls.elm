module EditorControls where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import JavaScript.Decode as JS exposing ((:=))
import Task exposing (..)
import Window


main =
  Varying.map2 view Window.width hints


-- VIEW

view : Int -> List Info -> Html
view outerWidth hints =
  let px = toString (outerWidth - 340) ++ "px"
  in
  div [ class "options" ]
    [ div [ class "hint", style [("width", px)] ] (viewHintList hints)
    , div
        [ class "button blue"
        , title "Compile and run the fresh code (Ctrl-Enter)"
        , onClick (Stream.message compileClicks.address ())
        ]
        [ text "Compile" ]
    , div
        [ class "button green"
        , title "Keep the state, change the behavior (Ctrl-Shift-Enter)"
        , onClick (Stream.message hotSwapClicks.address ())
        ]
        [ text "Hot Swap" ]
    , div
        [ class "button purple"
        , title "Switch editor color scheme"
        , onClick (Stream.message lightsClicks.address ())
        ]
        [ text "Lights" ]
    ]


viewHintList : List Info -> List Html
viewHintList hints =
  case hints of
    [] -> []
    _ ->
        text "Hint: " ::
          List.intersperse (text ", ") (List.map viewHint hints)


viewHint : Info -> Html
viewHint hint =
    a [ href hint.href, target "_top" ] [ text hint.name ]


-- INPUT / OUTPUT

input compileClicks : Stream.Input ()

foreign output compile : Stream ()
foreign output compile =
    compileClicks.stream


input hotSwapClicks : Stream.Input ()

foreign output hotSwap : Stream ()
foreign output hotSwap =
    hotSwapClicks.stream


input lightsClicks : Stream.Input ()

foreign output lights : Stream ()
foreign output lights =
    lightsClicks.stream


-- HINTS

type alias HintState =
    { rawDocs : Package
    , docs : Docs
    , hints : List Info
    }


type alias Docs = Dict.Dict String (List Info)


type alias Info =
    { name : String
    , href : String
    }


emptyState : HintState
emptyState =
    { rawDocs = []
    , docs = Dict.empty
    , hints = []
    }


packageToDocs : Package -> Docs
packageToDocs moduleList =
  let insert (token, info) dict =
          Dict.update token (\value -> Just (info :: Maybe.withDefault [] value)) dict
  in
      List.concatMap moduleToDocs moduleList
        |> List.foldl insert Dict.empty


moduleToDocs : Module -> List (String, Info)
moduleToDocs modul =
  let urlTo name =
        "http://package.elm-lang.org/packages/"
        ++ modul.packageName ++ "/latest/" ++ modul.name ++ "#" ++ name

      toKeyValue name =
        let fullName = modul.name ++ "." ++ name
            info = Info fullName (urlTo name)
        in
            [ (name, info)
            , (fullName, info)
            ]
  in
      modul.values.aliases ++ modul.values.types ++ modul.values.values
        |> List.concatMap toKeyValue



-- wiring

foreign input tokens : Stream (Maybe String)


hints : Varying (List Info)
hints =
  Stream.fold updateHint emptyState actions
    |> Varying.map .hints


actions : Stream Action
actions =
    Stream.merge
      (Stream.map CursorMove tokens)
      (Stream.map AddDocs docs.stream)


type Action
    = AddDocs Package
    | CursorMove (Maybe String)


updateHint : Action -> HintState -> HintState
updateHint action state =
  case action of
    AddDocs pkg ->
        let newRawDocs = pkg ++ state.rawDocs
        in
            { state |
                rawDocs <- newRawDocs,
                docs <- packageToDocs newRawDocs
            }

    CursorMove Nothing ->
        { state |
            hints <- []
        }

    CursorMove (Just name) ->
        { state |
            hints <-
                Maybe.withDefault []
                    (Dict.get name state.docs)
        }


-- DOCUMENTATION

input docs : Stream.Input Package


type alias Package = List Module

type alias Module =
    { packageName : String
    , name : String
    , values : Values
    }

type alias Values =
    { aliases : List String
    , types : List String
    , values : List String
    }


-- Documentation JSON

package : String -> JS.Decoder Package
package packageName =
  let name =
        "name" := JS.string

      values =
          JS.object3 Values
            ("aliases" := JS.list name)
            ("types" := JS.list name)
            ("values" := JS.list name)
  in
      JS.list (JS.object2 (Module packageName) name values)


-- Load Docs

foreign input init : Stream Int

input ignored : Stream (Result Http.Error (List ()))
input ignored from
    Stream.map (always loadDocs) init


loadDocs : Task Http.Error (List ())
loadDocs =
    sequence (List.map docsFor ["elm-lang/core", "evancz/elm-html", "evancz/elm-markdown"])


docsFor : String -> Task Http.Error ()
docsFor packageName =
  let url =
        "/packages/" ++ packageName ++ ".json"
  in
      Http.get (package packageName) url
        `andThen` Stream.send docs.address
