module Ui.Package exposing (..)


import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Html.Lazy as HL
import Data.Version as Version exposing (Version(..))
import Data.PackageList as PackageList exposing (Package)
import Dict exposing (Dict)
import Http
import Ui.Navigation
import Json.Decode as D
import Json.Encode as E
import FeatherIcons as Icon


type alias Model =
  { query : String
  , packages : PackageList.Packages
  }


init : ( Model, Cmd Msg )
init =
  ( { query = ""
    , packages = PackageList.preinstalled
    }
  , PackageList.fetch GotPackageList
  )


type Msg
  = GotPackageList (Result Http.Error (List PackageList.Package))
  | OnQuery String
  | OnInstall Package
  | OnInstalled Package (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotPackageList (Ok news) ->
      ( { model | packages = PackageList.fromNews news }
      , Cmd.none
      )

    GotPackageList (Err err) ->
      ( model -- TODO
      , Cmd.none
      )

    OnQuery query ->
      ( { model | query = query }
      , Cmd.none
      )

    OnInstall package ->
      ( { model | query = "" }
      , PackageList.attemptInstall OnInstalled package
      )

    OnInstalled package (Ok _) ->
      ( { model | packages = PackageList.toInstalled package model.packages }
      , Cmd.none
      )

    OnInstalled package (Err _) ->
      ( model -- TODO
      , Cmd.none
      )


view : Model -> Html Msg
view model =
  H.div
    [ HA.id "packages" ]
    [ H.div
        [ HA.id "packages__installer"]
        [ viewQuery model
        , viewAllFound model
        ]
    ]


viewQuery : Model -> Html Msg
viewQuery model =
  H.input
    [ HA.id "package-query"
    , HA.type_ "text"
    , HA.value model.query
    , HE.onInput OnQuery
    , HA.placeholder "author/project"
    ]
    []


viewAllFound : Model -> Html Msg
viewAllFound model =
  let results =
        if String.isEmpty model.query
        then PackageList.getInstalled model.packages
        else PackageList.getQuery model.query model.packages
  in
  HK.node "div" [ HA.id "package-options" ] (List.map viewFoundPackage results)


viewFoundPackage : ( Package, Maybe Version ) -> ( String, Html Msg )
viewFoundPackage ( package, installation ) =
  ( PackageList.toName package
  , HL.lazy viewFound ( package, installation )
  )


viewFound : ( Package, Maybe Version ) -> Html Msg
viewFound ( package, installation ) =
  let viewAction =
        case installation of
          Just installed ->
            if installed == package.version then
              [ viewVersion (Version.toString package.version)
              , viewUninstallButton (OnInstall package)
              ]
            else
              [ viewVersion (Version.toString installed ++ " → " ++ Version.toString package.version)
              , viewUpgradeButton (OnInstall package)
              ]

          Nothing ->
            [ viewVersion (Version.toString package.version)
            , viewInstallButton (OnInstall package)
            ]

      viewVersion string =
        H.div
          [ HA.class "package-option__version" ]
          [ H.text string ]
  in
  H.div
    [ HA.class "package-option" ]
    [ H.div
        [ HA.class "package-option__left" ]
        [ H.span [ HA.class "package-option__author" ] [ H.text package.author ]
        , H.text "/"
        , H.span [ HA.class "package-option__project" ] [ H.text package.project ]
        ]
    , H.div
        [ HA.class "package-option__right" ]
        viewAction
    ]


viewUpgradeButton : Msg -> Html Msg
viewUpgradeButton onClick =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = Icon.arrowUp
    , iconColor = Just "green"
    , label = Nothing
    , labelColor = Nothing
    , alt = "Upgrade"
    , onClick = Just onClick
    }


viewUninstallButton : Msg -> Html Msg
viewUninstallButton onClick =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = Icon.x
    , iconColor = Just "red"
    , label = Nothing
    , labelColor = Nothing
    , alt = "Uninstall"
    , onClick = Just onClick
    }


viewInstallButton : Msg -> Html Msg
viewInstallButton onClick =
  Ui.Navigation.iconButton []
    { background = Nothing
    , icon = Icon.plus
    , iconColor = Nothing
    , label = Nothing
    , labelColor = Nothing
    , alt = "Install"
    , onClick = Just onClick
    }

