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
import Ui.Icon
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
  | OnInstalled Package (Result Http.Error PackageList.Installation)


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
      ( { model | query = "", packages = PackageList.setInstallation package PackageList.Installing model.packages }
      , PackageList.attemptInstall OnInstalled package
      )

    OnInstalled package (Ok result) ->
      ( { model | packages = PackageList.setInstallation package result model.packages }
      , Cmd.none
      )

    OnInstalled package (Err _) ->
      ( { model | packages = PackageList.setInstallation package (PackageList.Failed "HTTP error" Nothing) model.packages }
      , Cmd.none
      )


view : List (H.Attribute Msg) -> Model -> Html Msg
view attrs model =
  let installed =
        PackageList.getInstalled model.packages
  in
  H.div
    (attrs ++ [ HA.id "packages" ])
    [ H.div
        [ HA.id "packages__installer" ]
        [ H.div
            [ HA.id "packages__installed" ]
            [ H.h3 [] [ H.text "Installed" ]
            , HK.node "div" [ HA.id "package-options" ] (List.map viewFoundPackage installed)
            ]
        , H.div
            [ HA.id "packages__registry" ]
            [ H.h3 [] [ H.text "Registry" ]
            , viewQuery model
            , viewAllFound model
            ]
        ]
    ]


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


viewAllFound : Model -> Html Msg
viewAllFound model =
  if String.length model.query < 3
  then HL.lazy viewPopular model.packages
  else viewSearchResults model


viewPopular : PackageList.Packages -> Html Msg
viewPopular packages =
  let popular =
        PackageList.getPopular packages
  in
  H.div []
    [ HK.node "div" [ HA.id "package-options" ] (List.map viewFoundPackage popular)
    , H.div [ HA.id "packages__popular-note" ]
        [ H.text ("Search to explore the other great packages.") ]
    ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
  let results =
        PackageList.fromQuery model.query model.packages
  in
  HK.node "div" [ HA.id "package-options" ] (List.map viewFoundPackage results)


viewFoundPackage : ( Package, PackageList.Installation ) -> ( String, Html Msg )
viewFoundPackage ( package, installation ) =
  ( PackageList.toName package
  , HL.lazy viewFound ( package, installation )
  )


viewFound : ( Package, PackageList.Installation ) -> Html Msg
viewFound ( package, installation ) =
  let viewAction =
        case installation of
          PackageList.NotInstalled ->
            [ viewVersion (Version.toString package.version)
            , viewInstallButton (OnInstall package)
            ]

          PackageList.Installing ->
            [ viewVersion (Version.toString package.version)
            , Ui.Icon.simpleIcon [ HA.style "padding-left" "10px" ] Nothing Icon.loader
            ]

          PackageList.Installed installed ->
            if installed == package.version then
              [ viewVersion (Version.toString package.version)
              , viewUninstallButton (OnInstall package)
              ]
            else
              [ viewVersion (Version.toString installed ++ " → " ++ Version.toString package.version)
              , viewUpgradeButton (OnInstall package)
              ]

          PackageList.Incompatible ->
            [ viewVersion (Version.toString package.version)
            , Ui.Icon.simpleIcon [ HA.style "padding-left" "10px" ] (Just "orange") Icon.alertCircle
            ]

          PackageList.Failed _ _ ->
            [ viewVersion (Version.toString package.version)
            , Ui.Icon.simpleIcon [ HA.style "padding-left" "10px" ] (Just "red") Icon.alertCircle
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
    , H.div [ HA.class "package-option__right" ] viewAction
    ]


viewUpgradeButton : Msg -> Html Msg
viewUpgradeButton onClick =
  Ui.Icon.button [ HA.style "padding-left" "10px" ]
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
  Ui.Icon.button [ HA.style "padding-left" "10px" ]
    { background = Nothing
    , icon = Icon.trash
    , iconColor = Nothing
    , label = Nothing
    , labelColor = Nothing
    , alt = "Uninstall"
    , onClick = Just onClick
    }


viewInstallButton : Msg -> Html Msg
viewInstallButton onClick =
  Ui.Icon.button [ HA.style "padding-left" "10px" ]
    { background = Nothing
    , icon = Icon.plus
    , iconColor = Nothing
    , label = Nothing
    , labelColor = Nothing
    , alt = "Install"
    , onClick = Just onClick
    }

