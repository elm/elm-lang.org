port module EditorControls exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Set
import String
import Task



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- FOREIGN VALUES


port tokens : (Maybe String -> msg) -> Sub msg

port rawImports : (List RawImport -> msg) -> Sub msg


port compile : () -> Cmd msg

port lights : () -> Cmd msg




-- MODEL


type alias Model =
    { docs : List ModuleDocs
    , imports : ImportDict
    , tokens : TokenDict
    , hints : List Hint
    }


init : (Model, Cmd Msg)
init =
  ( emptyModel
  , Task.attempt DocsLoaded getAllDocs
  )


emptyModel : Model
emptyModel =
  { docs = []
  , imports = defaultImports
  , tokens = Dict.empty
  , hints = []
  }



-- UPDATE


type Msg
  = DocsLoaded (Result Http.Error (List ModuleDocs))
  | UpdateImports ImportDict
  | CursorMove (Maybe String)
  | Compile
  | Lights


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DocsLoaded (Err _) ->
      ( model, Cmd.none )

    DocsLoaded (Ok pkg) ->
      let
        newDocs =
          pkg ++ model.docs
      in
        ( { model
            | docs = newDocs
            , tokens = toTokenDict model.imports newDocs
          }
        , Cmd.none
        )

    UpdateImports imports ->
      ( { model
          | imports = imports
          , tokens = toTokenDict imports model.docs
        }
      , Cmd.none
      )

    CursorMove Nothing ->
      ( { model | hints = [] }
      , Cmd.none
      )

    CursorMove (Just name) ->
      ( { model | hints = Maybe.withDefault [] (Dict.get name model.tokens) }
      , Cmd.none
      )

    Compile ->
      ( model
      , compile ()
      )

    Lights ->
      ( model
      , lights ()
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ tokens CursorMove
    , rawImports (UpdateImports << toImportDict)
    ]



-- VIEW


view : Model -> Html Msg
view {hints} =
  div [ class "options" ]
    [ div [ class "hint" ] (viewHintList hints)
    , div
        [ class "button blue"
        , title "Compile and run the fresh code (Ctrl-Enter)"
        , onClick Compile
        ]
        [ text "Compile" ]
    , div
        [ class "button yellow"
        , title "Switch editor color scheme"
        , onClick Lights
        ]
        [ text "Lights" ]
    ]


viewHintList : List Hint -> List (Html msg)
viewHintList hints =
  case hints of
    [] ->
      [ text "More Examples "
      , a [href "/examples", target "_blank"] [text "Here"]
      ]

    _ ->
      text "Docs: " ::
        List.intersperse (text ", ") (List.map viewHint hints)


viewHint : Hint -> Html msg
viewHint hint =
  a [ href hint.href, target "_blank" ] [ text hint.name ]



-- DOCUMENTATION


type alias ModuleDocs =
    { pkg : String
    , name : String
    , values : Values
    }


type alias Values =
    { aliases : List String
    , types : List (String, List String)
    , values : List String
    }


urlTo : ModuleDocs -> String -> String
urlTo {pkg,name} valueName =
  "http://package.elm-lang.org/packages/"
  ++ pkg ++ "/latest/" ++ dotToHyphen name ++ "#" ++ valueName


dotToHyphen : String -> String
dotToHyphen string =
  String.map (\c -> if c == '.' then '-' else c) string



-- FETCH DOCS


getAllDocs : Task.Task Http.Error (List ModuleDocs)
getAllDocs =
  let
    supportedPackages =
      [ "elm-lang/core"
      , "elm-lang/html"
      , "elm-lang/http"
      , "elm-lang/svg"
      , "evancz/elm-markdown"
      ]
  in
    supportedPackages
      |> List.map getDocs
      |> Task.sequence
      |> Task.map List.concat


getDocs : String -> Task.Task Http.Error (List ModuleDocs)
getDocs pkg =
  let
    url =
      "http://package.elm-lang.org/packages/" ++ pkg ++ "/latest/documentation.json"
  in
    Http.toTask (Http.get url (Json.list (moduleDecoder pkg)))


moduleDecoder : String -> Json.Decoder ModuleDocs
moduleDecoder pkg =
  let
    name =
      Json.field "name" Json.string

    tipe =
      Json.map2 (,)
        name
        (Json.field "cases" (Json.list (Json.index 0 Json.string)))

    values =
      Json.map3 Values
        (Json.field "aliases" (Json.list name))
        (Json.field "types" (Json.list tipe))
        (Json.field "values" (Json.list name))
  in
    Json.map2 (ModuleDocs pkg) name values



-- FORMAT DOCS


type alias TokenDict =
  Dict.Dict String (List Hint)


type alias Hint =
  { name : String
  , href : String
  }


toTokenDict : ImportDict -> List ModuleDocs -> TokenDict
toTokenDict imports moduleList =
  let
    getMaybeHints moduleDocs =
      Maybe.map (filteredHints moduleDocs) (Dict.get moduleDocs.name imports)

    insert (token, hint) dict =
      Dict.update token (\value -> Just (hint :: Maybe.withDefault [] value)) dict
  in
    moduleList
      |> List.filterMap getMaybeHints
      |> List.concat
      |> List.foldl insert Dict.empty


filteredHints : ModuleDocs -> Import -> List (String, Hint)
filteredHints moduleDocs importData =
  let
    allNames =
      moduleDocs.values.aliases
      ++ List.map Tuple.first moduleDocs.values.types
      ++ moduleDocs.values.values
  in
    List.concatMap (unionTagsToHints moduleDocs) moduleDocs.values.types
    ++ List.concatMap (nameToHints moduleDocs importData) allNames


nameToHints : ModuleDocs -> Import -> String -> List (String, Hint)
nameToHints moduleDocs {alias,exposed} name =
  let
    fullName =
      moduleDocs.name ++ "." ++ name

    hint =
      Hint fullName (urlTo moduleDocs name)

    localName =
      Maybe.withDefault moduleDocs.name alias ++ "." ++ name
  in
    if isExposed name exposed then
      [ (name, hint), (localName, hint) ]

    else
      [ (localName, hint) ]


unionTagsToHints : ModuleDocs -> (String, List String) -> List (String, Hint)
unionTagsToHints moduleDocs (tipeName, tags) =
  let
    addHints tag hints =
      let
        fullName =
          moduleDocs.name ++ "." ++ tag

        hint =
          Hint fullName (urlTo moduleDocs tipeName)
      in
        (tag, hint) :: (fullName, hint) :: hints
  in
    List.foldl addHints [] tags



-- IMPORTS


type alias RawImport =
  { name : String
  , alias : Maybe String
  , exposed : Maybe (List String)
  }


type alias ImportDict =
  Dict.Dict String Import


type alias Import =
  { alias : Maybe String
  , exposed : Exposed
  }


type Exposed
  = None
  | Some (Set.Set String)
  | All


isExposed : String -> Exposed -> Bool
isExposed name exposed =
  case exposed of
    None ->
      False

    Some set ->
      Set.member name set

    All ->
      True



-- CREATE IMPORT DICTS


toImportDict : List RawImport -> ImportDict
toImportDict rawImportList =
  Dict.union (Dict.fromList (List.map toImport rawImportList)) defaultImports


toImport : RawImport -> (String, Import)
toImport { name, alias, exposed } =
  let exposedSet =
          case exposed of
            Nothing -> None
            Just [".."] -> All
            Just vars -> Some (Set.fromList vars)
  in
      (name, Import alias exposedSet)


(=>) name exposed =
    (name, Import Nothing exposed)


defaultImports : ImportDict
defaultImports =
  Dict.fromList
    [ "Basics" => All
    , "Debug" => None
    , "List" => Some (Set.fromList ["List", "::"])
    , "Maybe" => Some (Set.singleton "Maybe")
    , "Result" => Some (Set.singleton "Result")
    , "Platform" => Some (Set.singleton "Program")
    , ("Platform.Cmd", Import (Just "Cmd") (Some (Set.fromList ["Cmd", "!"])))
    , ("Platform.Sub", Import (Just "Sub") (Some (Set.singleton "Sub")))
    ]
