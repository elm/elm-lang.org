module EditorControls where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import JavaScript.Decode as JS exposing ((:=))
import Port
import Set
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
        , onClick (Port.message compile.address ())
        ]
        [ text "Compile" ]
    , div
        [ class "button green"
        , title "Keep the state, change the behavior (Ctrl-Shift-Enter)"
        , onClick (Port.message hotSwap.address ())
        ]
        [ text "Hot Swap" ]
    , div
        [ class "button purple"
        , title "Switch editor color scheme"
        , onClick (Port.message lights.address ())
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
    a [ href hint.href, target "_blank" ] [ text hint.name ]


-- OUTBOUND PORTS

port compile : Port.OutboundPort ()
port hotSwap : Port.OutboundPort ()
port lights : Port.OutboundPort ()


-- HINTS

type alias HintState =
    { rawDocs : Package
    , docs : Docs
    , imports : Dict.Dict String Import
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
    , imports = defaultImports
    , hints = []
    }


toDocs : Dict.Dict String Import -> Package -> Docs
toDocs imports moduleList =
  let getInfo modul =
          Maybe.map (moduleToDocs modul) (Dict.get modul.name imports)

      insert (token, info) dict =
          Dict.update token (\value -> Just (info :: Maybe.withDefault [] value)) dict
  in
      List.filterMap getInfo moduleList
        |> List.concat
        |> List.foldl insert Dict.empty


moduleToDocs : Module -> Import -> List (String, Info)
moduleToDocs modul { alias, exposed } =
  let urlTo name =
        "http://package.elm-lang.org/packages/"
        ++ modul.packageName ++ "/latest/" ++ modul.name ++ "#" ++ name

      nameToPair name =
        let fullName = modul.name ++ "." ++ name
            info = Info fullName (urlTo name)
            localName = Maybe.withDefault modul.name alias ++ "." ++ name
            pairs = [(localName, info)]
        in
            case exposed of
              None ->
                  pairs

              Some set ->
                  if Set.member name set then (name, info) :: pairs else pairs

              All ->
                  (name, info) :: pairs

      typeToPair type' tag =
        let fullName = modul.name ++ "." ++ tag
            info = Info fullName (urlTo type')
        in
            [ (tag, info)
            , (fullName, info)
            ]
  in
      List.concatMap nameToPair (modul.values.aliases ++ modul.values.values)
      ++ List.concatMap (\(type', tags) -> List.concatMap (typeToPair type') tags) modul.values.types



-- wiring

port tokens : Port.InboundPort (Maybe String)


hints : Varying (List Info)
hints =
  Stream.fold updateHint emptyState actions
    |> Varying.map .hints


actions : Stream Action
actions =
    Stream.mergeMany
      [ Stream.map CursorMove tokens.stream
      , Stream.map AddDocs docs.stream
      , Stream.map UpdateImports imports
      ]


type Action
    = AddDocs Package
    | UpdateImports (Dict.Dict String Import)
    | CursorMove (Maybe String)


updateHint : Action -> HintState -> HintState
updateHint action state =
  case action of
    AddDocs pkg ->
        let newRawDocs = pkg ++ state.rawDocs
        in
            { state |
                rawDocs <- newRawDocs,
                docs <- toDocs state.imports newRawDocs
            }

    UpdateImports imports ->
        { state |
            imports <- imports,
            docs <- toDocs imports state.rawDocs
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

port docs : Port.Port Package


type alias Package = List Module

type alias Module =
    { packageName : String
    , name : String
    , values : Values
    }

type alias Values =
    { aliases : List String
    , types : List (String, List String)
    , values : List String
    }


-- Documentation JSON

package : String -> JS.Decoder Package
package packageName =
  let name =
          "name" := JS.string

      type' =
          JS.object2 (,) name
            ("cases" := JS.list (JS.tuple2 always JS.string JS.value))

      values =
          JS.object3 Values
            ("aliases" := JS.list name)
            ("types" := JS.list type')
            ("values" := JS.list name)
  in
      JS.list (JS.object2 (Module packageName) name values)


-- Load Docs

perform
  sequence (List.map docsFor ["elm-lang/core", "evancz/elm-html", "evancz/elm-markdown"])


docsFor : String -> Task Http.Error ()
docsFor packageName =
  let url =
        "/packages/" ++ packageName ++ ".json"
  in
      Http.get (package packageName) url
        `andThen` Port.send docs.address


-- IMPORTS

imports : Stream (Dict.Dict String Import)
imports =
  let toDict list =
        Dict.union (Dict.fromList (List.map toImport list)) defaultImports
  in
      Stream.map toDict rawImports.stream


(=>) name exposed =
    (name, Import Nothing exposed)


defaultImports : Dict.Dict String Import
defaultImports =
  Dict.fromList
    [ "Basics" => All
    , "List" => Some (Set.fromList ["List", "::"])
    , "Maybe" => Some (Set.singleton "Maybe")
    , "Result" => Some (Set.singleton "Result")
    , "Stream" => Some (Set.singleton "Stream")
    , "Varying" => Some (Set.singleton "Varying")
    ]


port rawImports : Port.InboundPort (List RawImport)

type alias RawImport =
    { name : String
    , alias : Maybe String
    , exposed : Maybe (List String)
    }


type alias Import =
    { alias : Maybe String, exposed : Exposed }


type Exposed = None | Some (Set.Set String) | All


toImport : RawImport -> (String, Import)
toImport { name, alias, exposed } =
  let exposedSet =
          case exposed of
            Nothing -> None
            Just [".."] -> All
            Just vars -> Some (Set.fromList vars)
  in
      (name, Import alias exposedSet)
