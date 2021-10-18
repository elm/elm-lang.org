module Ui.Package exposing (..)


import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Html.Lazy as HL
import Data.Version as V
import Data.Registry.Package as Package
import Data.Registry.Status as Status
import Data.Registry.Solution as Solution
import Data.Registry.Defaults as Defaults
import Data.Registry as Registry
import Data.Analytics as Analytics
import Data.Problem as Problem
import Data.Fetched as Fetched
import Data.Http
import Http
import Dict
import Process
import Task
import Ui.Navigation
import Ui.Icon
import Json.Decode as D
import Json.Encode as E
import FeatherIcons as Icon



type alias Model =
  { query : String
  , registry : Fetched.Fetched Registry.Registry
  , hash : Maybe String
  , debounce : Int
  }


init : ( Model, Cmd Msg )
init =
  ( { query = ""
    , registry = Fetched.Loading
    , hash = Nothing
    , debounce = 0
    }
  , Registry.fetch GotRegistry
  )


width : Int
width =
  350


widthPx : String
widthPx =
  String.fromInt width ++ "px"


getRegistry : Model -> Registry.Registry
getRegistry model =
  case model.registry of
    Fetched.Loading -> Registry.initial
    Fetched.Failed _ -> Registry.initial
    Fetched.Success registry -> registry


getSolution : Model -> Solution.Solution
getSolution model =
  Solution.toSolution (Dict.values (getRegistry model))
    |> \solution -> { solution | hash = model.hash }


getProblems : Model -> Maybe Problem.Problems
getProblems model =
  Registry.getErrors (getRegistry model)
    |> List.filterMap Data.Http.onlyDecodedErrors -- TODO
    |> Problem.toManyIndexedProblems
    |> Problem.init


dismissAll : Model -> Model
dismissAll model =
  Tuple.first <| updateRegistry model <| \registry ->
    ( model, Registry.dismissAll registry, Cmd.none )



-- UPDATE


type Msg
  = GotRegistry (Result Http.Error (List Package.Package))
  | OnQuery String
  | OnDebounce
  | OnInstall Package.Package
  | OnUninstall Package.Package
  | OnDismiss Package.Package
  | OnEdited Package.Package (Result Status.Error Solution.Solution)
  | OnReportResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotRegistry (Ok news) ->
      ( { model | registry = Fetched.Success (Registry.fromNews news Registry.initial) }
      , Cmd.none
      )

    GotRegistry (Err (Http.BadBody errMsg as err)) ->
      ( { model | registry = Fetched.Failed err }
      , Analytics.reportError OnReportResult errMsg
      )

    GotRegistry (Err err) ->
      ( { model | registry = Fetched.Failed err }
      , Cmd.none
      )

    OnQuery query ->
      ( { model | query = query, debounce = model.debounce + 1 }
      , Task.perform (\_ -> OnDebounce) (Process.sleep 300)
      )

    OnDebounce ->
      ( { model | debounce = model.debounce - 1 }
      , Cmd.none
      )

    OnInstall package ->
      updateRegistry model <| \registry ->
        ( model
        , Registry.setStatus package Status.Loading registry
        , Registry.attemptEdit Registry.Install (OnEdited package) registry package
        )

    OnUninstall package ->
      updateRegistry model <| \registry ->
        ( model
        , Registry.setStatus package Status.Loading registry
        , Registry.attemptEdit Registry.Uninstall (OnEdited package) registry package
        )

    OnDismiss package ->
      updateRegistry model <| \registry ->
        ( model
        , Registry.setStatus package Status.NotInstalled registry
        , Cmd.none
        )

    OnEdited package (Ok solution) ->
      updateRegistry model <| \registry ->
        ( { model | hash = solution.hash }
        , registry
            |> Registry.setStatus package Status.NotInstalled
            |> Registry.fromSolution solution
        , Cmd.none
        )

    OnEdited package (Err err) ->
      updateRegistry model <| \registry ->
        ( model
        , Registry.setStatus package (Status.Failed err) registry
        , Cmd.none
        )

    OnReportResult _ ->
      ( model, Cmd.none )


updateRegistry : Model -> (Registry.Registry -> ( Model, Registry.Registry, Cmd Msg )) -> ( Model, Cmd Msg )
updateRegistry model updater =
  case model.registry of
    Fetched.Loading ->
      ( model, Cmd.none )

    Fetched.Failed _ ->
      ( model, Cmd.none )

    Fetched.Success registry ->
      let ( newModel, newRegistry, cmd ) = updater registry in
      ( { newModel | registry = Fetched.Success newRegistry }, cmd )



-- VIEW


view : List (H.Attribute Msg) -> Model -> Html Msg
view attrs model =
  let direct =
        getRegistry model
          |> Registry.filterStatus Status.isDirectDep
          |> Registry.getValues

      failed =
        getRegistry model
          |> Registry.filterStatus Status.isFailed
          |> Registry.getValues
  in
  H.div
    (attrs ++ [ HA.id "packages" ])
    [ H.div
        [ HA.id "packages__installer"
        , HA.style "min-width" widthPx
        ] <|
        case model.registry of
          Fetched.Loading ->
            [ Ui.Icon.simpleIcon [] Nothing Icon.loader ]

          Fetched.Failed err ->
            [ H.div
                [ HA.style "font-size" "12px" ]
                [ Ui.Icon.simpleIcon [ HA.style "padding-right" "5px" ] (Just "red") Icon.alertCircle
                , H.text "Could not fetch packages! Please try again later."
                ]
            ]

          Fetched.Success registry ->
            [ H.div
                [ HA.id "packages__installed" ]
                [ H.h3 [] [ H.text "Installed" ]
                , viewKeyedContainer [] (List.map viewKeyedPackage direct)
                , viewKeyedContainer [ HA.style "margin-top" "10px" ] (List.map viewKeyedPackage failed)
                ]

            , H.div
                [ HA.id "packages__registry" ]
                [ H.h3 [] [ H.text "Registry" ]
                , viewQuery model
                , if String.isEmpty model.query
                    then HL.lazy viewPopular registry
                    else if model.debounce /= 0 then Ui.Icon.simpleIcon [] Nothing Icon.loader
                    else viewSearchResults model registry
                ]
            ]
    ]



-- VIEW POPULAR


viewPopular : Registry.Registry -> Html Msg
viewPopular registry =
  let popular =
        registry
          |> Registry.filterStatus Status.isSearchable
          |> Registry.filterKeys Defaults.popular
  in
  H.div []
    [ viewKeyedContainer [] (List.map viewKeyedPackage popular)
    ]



-- VIEW SEARCH RESULTS


viewQuery : Model -> Html Msg
viewQuery model =
  H.input
    [ HA.id "package-query"
    , HA.type_ "text"
    , HA.value model.query
    , HE.onInput OnQuery
    , HA.placeholder "Search"
    ]
    []


viewSearchResults : Model -> Registry.Registry -> Html Msg
viewSearchResults model registry =
  let results =
        registry
          |> Registry.filterStatus Status.isSearchable
          |> Registry.getValues
          |> Registry.search model.query
  in
  viewKeyedContainer [] (List.map viewKeyedPackage results)



-- KEYED CONTAINER


viewKeyedContainer : List (H.Attribute msg) -> List ( String, Html msg ) -> Html msg
viewKeyedContainer attrs =
  HK.node "div" (attrs ++ [ HA.id "package-options" ])


viewKeyedPackage : ( Package.Package, Status.Status ) -> ( String, Html Msg )
viewKeyedPackage ( package, state ) =
  ( Package.toName package
  , HL.lazy viewPackage ( package, state )
  )



-- VIEW PACKAGE


viewPackage : ( Package.Package, Status.Status ) -> Html Msg
viewPackage ( package, state ) =
  H.div [ HA.class "package-option" ] <|
    case state of
      Status.NotInstalled ->
        viewOkPackage package
          [ viewPackageVersion package.version
          , viewButtonIcon Nothing "Install" Icon.plus (OnInstall package)
          ]

      Status.IndirectDep version ->
        viewOkPackage package
          [ viewPackageVersion version
          , viewButtonIcon Nothing "Install" Icon.plus (OnInstall package)
          ]

      Status.Loading ->
        viewOkPackage package
          [ viewPackageVersion package.version
          , viewSimpleIcon Nothing Icon.loader
          ]

      Status.DirectDep version ->
        viewOkPackage package
          [ viewPackageVersion version
          , if List.member (Package.toKey package) Defaults.locked
            then viewSimpleIcon Nothing Icon.lock
            else viewButtonIcon Nothing "Uninstall" Icon.trash (OnUninstall package)
          ]

      Status.Failed err ->
        viewErrorPackage package err


viewOkPackage : Package.Package -> List (Html Msg) -> List (Html Msg)
viewOkPackage pkg actions =
  [ H.div [ HA.class "package-option__left" ] [ viewPackageName pkg ]
  , H.div [ HA.class "package-option__right" ] actions
  ]


viewErrorPackage : Package.Package -> Status.Error -> List (Html Msg)
viewErrorPackage package error =
  let viewErrorIcon =
        Ui.Icon.simpleIcon
          [ HA.style "padding-right" "5px"
          , HA.style "top" "1px"
          , HA.style "width" "20px"
          ]
          (Just "red")
          Icon.alertCircle
  in
  [ H.div
      [ HA.class "package-option__left"
      , HA.style "display" "flex"
      , HA.style "font-size" "12px"
      ]
      [ viewErrorIcon
      , H.div
          [ HA.class "package-option__error" ]
          [ H.text "Could not install "
          , H.span
              [ HA.style "font-weight" "bold" ]
              [ viewPackageDocsLink package <|
                  Package.toName package ++ " " ++ V.toString package.version
              ]
          , H.text "."
          ]
      ]
  , H.div
      [ HA.class "package-option__right" ]
      [ viewButtonIcon Nothing "Dismiss" Icon.x (OnDismiss package) ]
  ]


viewPackageName : Package.Package -> Html Msg
viewPackageName pkg =
  viewPackageDocsLink pkg (Package.toName pkg)


viewPackageDocsLink : Package.Package -> String -> Html Msg
viewPackageDocsLink pkg str =
  H.a
    [ HA.target "_blank"
    , HA.href (Package.toDocsLink pkg)
    ]
    [ H.text str ]


viewPackageVersion : V.Version -> Html Msg
viewPackageVersion version =
  H.div
    [ HA.class "package-option__version" ]
    [ H.text (V.toString version) ]



-- HELPERS


viewSimpleIcon : Maybe String -> Icon.Icon -> Html msg
viewSimpleIcon =
  Ui.Icon.simpleIcon
    [ HA.style "padding-left" "10px"
    , HA.style "top" "1px"
    ]


viewButtonIcon : Maybe String -> String -> Icon.Icon -> msg -> Html msg
viewButtonIcon iconColor alt icon onClick =
  Ui.Icon.button [ HA.style "padding-left" "10px" ]
    { background = Nothing
    , icon = icon
    , iconColor = iconColor
    , label = Nothing
    , labelColor = Nothing
    , alt = alt
    , onClick = Just onClick
    }