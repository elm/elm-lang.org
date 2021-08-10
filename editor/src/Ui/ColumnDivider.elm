module Ui.ColumnDivider exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onMouseOver, onMouseLeave)
import Html.Lazy exposing (..)
import Json.Encode as E
import Json.Decode as D
import Data.Window exposing (Window)



type alias Model =
  { current : Float
  , movement : Movement
  }


type Movement
  = None
  | Moving Float Bool


isUpperLimit : Window -> Model -> Bool
isUpperLimit window model =
  model.current >= upperLimit window


lowerLimit : Float
lowerLimit =
  35


upperLimit : Window -> Float
upperLimit window =
  window.width - lowerLimit


halfPoint : Window -> Float
halfPoint window =
  window.width / 2


clamp : Window -> Float -> Float
clamp window =
  Basics.max lowerLimit >> Basics.min (upperLimit window)


jump : Window -> Float -> Float
jump window current =
  if current <= lowerLimit then upperLimit window
  else if current >= upperLimit window then halfPoint window
  else if current > halfPoint window then upperLimit window
  else lowerLimit


isSignificant : Float -> Float -> Bool
isSignificant initial current =
  abs (initial - current) < 4


isMoving : Model -> Bool
isMoving model =
  case model.movement of
    Moving _ _ -> True
    None -> False



-- INIT


init : Window -> Model
init window =
  { current = window.width / 2
  , movement = None
  }


type Msg
  = OnDown
  | OnMove Float
  | OnUp Float
  | OnClick
  | OnClickLeft


update : Window -> Msg -> Model -> Model
update window msg model =
  case msg of
    OnDown ->
      { model | movement = Moving model.current False }

    OnMove latest ->
      case model.movement of
        Moving initial True ->
          { model | current = clamp window latest }

        Moving initial False ->
          if isSignificant initial latest
          then { model | current = clamp window latest }
          else { model | current = clamp window latest, movement = Moving initial True }

        None ->
          { model | current = clamp window latest }

    OnUp latest ->
      case model.movement of
        Moving _ True ->
          { model | movement = None, current = clamp window latest }

        Moving initial False ->
          { model | movement = None, current = jump window initial }

        None ->
          { model | movement = None, current = jump window model.current }

    OnClick ->
      { model | movement = None, current = jump window model.current }

    OnClickLeft ->
      if model.current <= lowerLimit then
        { model | current = upperLimit window }
      else
        model


view : (Msg -> msg) -> Window -> Model -> List (Html msg) -> List (Html msg) -> Html msg
view onMsg window model leftChildren rightChildren =
  div
    [ id "double-pane"
    , style "width" "100%"
    , style "display" "flex"
    ]
    [ viewLeft onMsg window model leftChildren
    , Html.map onMsg (viewDivider window model)
    , viewRight window model rightChildren
    ]


viewLeft : (Msg -> msg) -> Window -> Model -> List (Html msg) -> Html msg
viewLeft onMsg window model =
  div
    [ id "left-side"
    , onClick (onMsg OnClickLeft)
    , style "width" (String.fromFloat model.current ++ "px")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewRight : Window -> Model -> List (Html msg) -> Html msg
viewRight window model =
  div
    [ id "right-side"
    , style "width" (String.fromFloat (window.width - model.current) ++ "px")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewDivider : Window -> Model -> Html Msg
viewDivider window model =
  node "column-divider"
    [ on "down" (D.succeed OnDown)
    , on "move" (D.map OnMove decodePixels)
    , on "up" (D.map OnUp decodePixels)
    , on "_click" (D.succeed OnClick)
    , property "pixels" (E.float model.current)
    , style "width" (if isUpperLimit window model then "40px" else "10px")
    , style "left" (String.fromFloat model.current ++ "px")
    ]
    []


decodePixels : D.Decoder Float
decodePixels =
  D.at [ "target", "pixels" ] D.float
