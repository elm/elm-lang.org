module Ui.Package exposing (..)


import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Html.Lazy as HL
import Data.Version as Version exposing (Version(..))
import Data.Package as Package exposing (Package)
import Dict exposing (Dict)
import Http
import Ui.Navigation
import Json.Decode as D
import FeatherIcons as Icon


type alias Model =
  { query : String
  , packages : Dict String ( Package, Maybe Version )
  }


getInstalled : Model -> List Package
getInstalled model =
  model.packages
    |> Dict.values
    |> List.filterMap (\(p, i) -> if Package.isInstalled i then Just p else Nothing)


type Packages
  = Loading
  | Success (Dict String Package)
  | Failed Http.Error


init : ( Model, Cmd Msg )
init =
  ( { query = ""
    , packages =
        let installed = Package.toDict Package.defaults in
        Package.merge installed installed
    }
  , fetchAllPackages
  )


fetchAllPackages : Cmd Msg
fetchAllPackages =
  Http.get
    { url = "https://package.elm-lang.org/search.json"
    , expect = Http.expectJson GotAllPackages (D.list Package.decoder)
    }


type Msg
  = GotAllPackages (Result Http.Error (List Package))
  | OnQuery String
  | OnInstall Package


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotAllPackages (Ok all) ->
      ( { model | packages = Package.merge (Dict.map (always Tuple.first) model.packages) (Package.toDict all) }
      , Cmd.none
      )

    GotAllPackages (Err err) ->
      ( model -- TODO
      , Cmd.none
      )

    OnQuery query ->
      ( { model | query = query }
      , Cmd.none
      )

    OnInstall package ->
      ( { model | query = ""
        , packages = Dict.insert (Package.toName package) ( package, Just package.version ) model.packages
        }
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
        if String.isEmpty model.query then
          model.packages
            |> Dict.values
            |> List.filter (\( p, installation ) -> Package.isInstalled installation)
            |> List.sortBy (\( p, installation ) -> p.order )
        else
          model.packages
            |> Dict.values
            |> Package.search model.query
            |> List.sortBy (\( p, installation ) -> ( if Package.isInstalled installation then 0 else 1, p.order ) )
  in
  HK.node "div" [ HA.id "package-options" ] (List.map viewFoundPackage results)


viewFoundPackage : ( Package, Maybe Version ) -> ( String, Html Msg )
viewFoundPackage ( package, installation ) =
  ( Package.toName package
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

