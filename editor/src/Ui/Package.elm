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
  , all : Packages
  }


type Packages
  = Loading
  | Success (List Package)
  | Failed Http.Error


init : ( Model, Cmd Msg )
init =
  ( { query = ""
    , all = Loading
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


update : Msg -> Model -> Dict String Version -> ( Model, Dict String Version, Cmd Msg )
update msg model packages =
  case msg of
    GotAllPackages (Ok all) ->
      ( { model | all = Success all }
      , packages
      , Cmd.none
      )

    GotAllPackages (Err err) ->
      ( { model | all = Failed err }
      , packages
      , Cmd.none
      )

    OnQuery query ->
      ( { model | query = query }
      , packages
      , Cmd.none
      )

    OnInstall package ->
      ( { model | query = "" }
      , Dict.insert package.name package.version packages
      , Cmd.none
      )


view : Dict String Version -> Model -> Html Msg
view packages model =
  H.div
    [ HA.id "packages" ]
    [ H.div
        [ HA.id "packages__installed__container" ]
        [ H.h4 [ HA.id "packages__installed__title" ] [ H.text "Installed" ]
        , H.div [ HA.id "packages__installed" ] <| List.map viewInstalled (Dict.toList packages)
        ]
    , H.div [ HA.id "packages__installer"]
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
    , HA.placeholder "Filter packages..."
    ]
    []


viewAllFound : Model -> Html Msg
viewAllFound model =
  case model.all of
    Loading ->
      H.text "Loading..."

    Failed _ ->
      H.text "Could not load packages."

    Success all ->
      let results =
            Package.search model.query all
      in
      HK.node "div" [ HA.id "package-options" ] <| List.map viewFoundPackage results


viewFoundPackage : Package -> ( String, Html Msg )
viewFoundPackage package =
  ( package.name
  , HL.lazy viewFound package
  )


viewFound : Package -> Html Msg
viewFound package =
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
        [ H.div [ HA.class "package-option__version" ] [ H.text (Version.toString package.version) ]
        , viewInstallButton (OnInstall package)
        ]
    ]


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


viewInstalled : ( String, Version ) -> Html Msg
viewInstalled ( name, version ) =
  H.div [ HA.class "package-option__installed" ]
    [ H.div [ HA.class "package-option__installed__name" ] [ H.text name ]
    , H.div [ HA.class "package-option__installed__version" ] [ H.text (Version.toString version) ]
    ]
