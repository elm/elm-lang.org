module Ui.Package exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data.Version as Version exposing (Version(..))
import Dict exposing (Dict)


type alias Model =
  { package : String
  , version : String
  }


init : Model
init =
  { package = ""
  , version = ""
  }


type Msg
  = OnInputPackage String
  | OnInputVersion String
  | OnInstall


update : Msg -> Model -> Dict String Version -> ( Model, Dict String Version, Cmd Msg )
update msg model packages =
  case msg of
    OnInputPackage package ->
      ( { model | package = package }
      , packages
      , Cmd.none
      )

    OnInputVersion version ->
      ( { model | version = version }
      , packages
      , Cmd.none
      )

    OnInstall ->
      let version =
            Version.fromString model.version
              |> Maybe.withDefault (Version 1 0 0)
      in
      ( { model | package = "", version = "" }
      , Dict.insert model.package version packages
      , Cmd.none
      )


view : Dict String Version -> Model -> Html Msg
view packages model =
  div
    [ id "packages" ]
    [ viewInputPackage model
    , viewInputVersion model
    , viewConfirm
    , viewAllInstalled packages
    ]


viewInputPackage : Model -> Html Msg
viewInputPackage model =
  input [ type_ "text", onInput OnInputPackage, value model.package ] []


viewInputVersion : Model -> Html Msg
viewInputVersion model =
  input [ type_ "text", onInput OnInputVersion, value model.version ] []


viewConfirm : Html Msg
viewConfirm =
  button [ onClick OnInstall ] [ text "Install" ]


viewAllInstalled : Dict String Version -> Html Msg
viewAllInstalled packages =
  div [] <| List.map viewInstalled (Dict.toList packages)


viewInstalled : ( String, Version ) -> Html Msg
viewInstalled ( name, version ) =
  div [] [ text name, text (Version.toString version) ]