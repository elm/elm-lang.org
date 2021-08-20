module Ui.ColumnDivider exposing (Model, isRightMost, init, Msg, update, view)


{-| Control the sizes of the two columns, editor and result.

Relies on column-divider.js being present.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, preventDefaultOn)
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
  | Moving Float Float Bool


isRightMost : Window -> Model -> Bool
isRightMost window model =
  model.percent >= rightMost window


leftMost :  Window -> Float
leftMost window =
  toPercentage window 35


rightMost : Window -> Float
rightMost window =
  100 - toPercentage window (if window.width <= 1000 then 20 else 35)


halfPoint : Float
halfPoint =
  50


clamp : Window -> Float -> Float
clamp window =
  Basics.max (leftMost window) >> Basics.min (rightMost window)


jump : Window -> Float -> Float
jump window percent =
  if percent >= rightMost window then leftMost window
  else if percent <= leftMost window then (if window.width <= 1000 then leftMost window else halfPoint)
  else if percent >= halfPoint then rightMost window
  else leftMost window


isSignificant : Float -> Float -> Bool
isSignificant initial latest =
  abs (initial - latest) > 4


isMoving : Model -> Bool
isMoving model =
  case model.movement of
    Moving _ _ _ -> True
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
      { model | movement = Moving model.percent initial False }

    OnMove latest ->
      case model.movement of
        Moving percent initial False ->
          if isSignificant initial latest
          then { model | percent = toPercentage window latest, movement = Moving percent initial True }
          else { model | percent = toPercentage window latest }

        Moving _ _ True ->
          { model | percent = toPercentage window latest }

        None ->
          { model | percent = toPercentage window latest }

    OnUp latest ->
      case model.movement of
        Moving _ _ True ->
          { model | movement = None, percent = toPercentage window latest }

        Moving percent _ False ->
          { model | movement = None, percent = jump window percent }

        None ->
          { model | movement = None, percent = jump window model.percent }

    OnClick ->
      { model | movement = None, percent = jump window model.percent }

    OnClickLeft ->
      { model | percent = rightMost window }


view : (Msg -> msg) -> Window -> Model -> List (Html msg) -> List (Html msg) -> Html msg
view onMsg window model leftChildren rightChildren =
  let percent =
        clamp window model.percent
  in
  div
    [ id "double-pane"
    , style "width" "100%"
    , style "display" "flex"
    ]
    [ viewLeft onMsg window model percent leftChildren
    , Html.map onMsg (viewDivider window model percent)
    , viewRight window model percent rightChildren
    ]


viewLeft : (Msg -> msg) -> Window -> Model -> Float -> List (Html msg) -> Html msg
viewLeft onMsg window model percent =
  let events =
        if percent <= leftMost window then
          [ preventDefaultOn "touchend" (D.succeed ( onMsg OnClickLeft, True ))
          , onClick (onMsg OnClickLeft)
          ]
        else
          []
  in
  div <|
    [ id "left-side"
    , style "width" (String.fromFloat percent ++ "%")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ] ++ events


viewRight : Window -> Model -> Float -> List (Html msg) -> Html msg
viewRight window model percent =
  div
    [ id "right-side"
    , style "width" (String.fromFloat (100 - percent) ++ "%")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewDivider : Window -> Model -> Float -> Html Msg
viewDivider window model percent =
  node "column-divider"
    [ on "down" (D.map OnDown decodePixels)
    , on "move" (D.map OnMove decodePixels)
    , on "up" (D.map OnUp decodePixels)
    , on "_click" (D.succeed OnClick)
    , property "pixels" (E.float (fromPercentage window percent))
    , style "width" (if isRightMost window model then "40px" else "10px")
    , style "left" (String.fromFloat percent ++ "%")
    ]
    []


decodePixels : D.Decoder Float
decodePixels =
  D.at [ "target", "pixels" ] D.float
