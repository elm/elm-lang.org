module Ui.ColumnDivider exposing (Model, isUpperLimit, init, Msg, update, view)


{-| Control the sizes of the two columns, editor and result.

Relies on column-divider.js being present.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onMouseOver, onMouseLeave)
import Html.Lazy exposing (..)
import Json.Encode as E
import Json.Decode as D
import Data.Window exposing (Window)



type alias Model =
  { percent : Float
  , movement : Movement
  }


type Movement
  = None
  | Moving Float Bool


isUpperLimit : Window -> Model -> Bool
isUpperLimit window model =
  model.percent >= upperLimit window


lowerLimit :  Window -> Float
lowerLimit window =
  toPercentage window 35


upperLimit : Window -> Float
upperLimit window =
  100 - lowerLimit window


halfPoint : Float
halfPoint =
  50


clamp : Window -> Float -> Float
clamp window =
  toPercentage window >> Basics.max (lowerLimit window) >> Basics.min (upperLimit window)


jump : Window -> Float -> Float
jump window percent =
  if percent <= lowerLimit window then upperLimit window
  else if percent >= upperLimit window then halfPoint
  else if percent > halfPoint then upperLimit window
  else lowerLimit window


isSignificant : Float -> Float -> Bool
isSignificant initial latest =
  abs (initial - latest) > 4


isMoving : Model -> Bool
isMoving model =
  case model.movement of
    Moving _ _ -> True
    None -> False


toPercentage : Window -> Float -> Float
toPercentage window percent =
  percent * 100 / toFloat window.width


fromPercentage : Window -> Float -> Float
fromPercentage window percent =
  percent * toFloat window.width / 100



-- INIT


init : Window -> Model
init window =
  { percent = 50
  , movement = None
  }


type Msg
  = OnDown Float
  | OnMove Float
  | OnUp Float
  | OnClick
  | OnClickLeft


update : Window -> Msg -> Model -> Model
update window msg model =
  case msg of
    OnDown initial ->
      { model | movement = Moving initial False }

    OnMove latest ->
      case model.movement of
        Moving initial False ->
          if isSignificant initial latest
          then { model | percent = clamp window latest, movement = Moving initial True }
          else { model | percent = clamp window latest }

        Moving _ True ->
          { model | percent = clamp window latest }

        None ->
          { model | percent = clamp window latest }

    OnUp latest ->
      case model.movement of
        Moving _ True ->
          { model | movement = None, percent = clamp window latest }

        Moving _ False ->
          { model | movement = None, percent = jump window model.percent }

        None ->
          { model | movement = None, percent = jump window model.percent }

    OnClick ->
      { model | movement = None, percent = jump window model.percent }

    OnClickLeft ->
      if model.percent <= lowerLimit window then
        { model | percent = upperLimit window }
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
    , style "width" (String.fromFloat model.percent ++ "%")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewRight : Window -> Model -> List (Html msg) -> Html msg
viewRight window model =
  div
    [ id "right-side"
    , style "width" (String.fromFloat (100 - model.percent) ++ "%")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewDivider : Window -> Model -> Html Msg
viewDivider window model =
  node "column-divider"
    [ on "down" (D.map OnDown decodePixels)
    , on "move" (D.map OnMove decodePixels)
    , on "up" (D.map OnUp decodePixels)
    , on "_click" (D.succeed OnClick)
    , property "pixels" (E.float (fromPercentage window model.percent))
    , style "width" (if isUpperLimit window model then "40px" else "10px")
    , style "left" (String.fromFloat model.percent ++ "%")
    ]
    []


decodePixels : D.Decoder Float
decodePixels =
  D.at [ "target", "pixels" ] D.float
