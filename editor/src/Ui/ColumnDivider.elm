module Ui.ColumnDivider exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onMouseOver, onMouseLeave)
import Html.Lazy exposing (..)
import Json.Encode as E
import Json.Decode as D
import Data.Window exposing (Window)



type alias Model =
  { pixels : Float
  , movingFrom : Maybe Float
  , isSignificant : Bool
  }


isTooSmall : Model -> Bool
isTooSmall model =
  model.pixels <= lowerLimit


isTooLarge : Window -> Model -> Bool
isTooLarge window model =
  model.pixels >= upperLimit window


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
jump window pixels =
  if pixels <= lowerLimit then upperLimit window
  else if pixels >= upperLimit window then halfPoint window
  else if pixels > halfPoint window then upperLimit window
  else lowerLimit


isMoving : Model -> Bool
isMoving model =
  case model.movingFrom of
    Just _ -> True
    Nothing -> False



-- INIT


init : Window -> Model
init window =
  { pixels = window.width / 2
  , movingFrom = Nothing
  , isSignificant = False
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
      { model | movingFrom = Just model.pixels, isSignificant = False }

    OnMove px ->
      if model.isSignificant then
        { model | pixels = clamp window px }
      else
        case model.movingFrom of
          Just initial ->
            if abs (initial - px) < 4
            then { model | pixels = clamp window px }
            else { model | pixels = clamp window px, isSignificant = True }

          Nothing ->
            { model | pixels = clamp window px }

    OnUp px ->
      if model.isSignificant
      then { model | movingFrom = Nothing, pixels = clamp window px }
      else { model | movingFrom = Nothing, pixels = jump window (Maybe.withDefault px model.movingFrom) }

    OnClick ->
      { model | movingFrom = Nothing, pixels = jump window model.pixels }

    OnClickLeft ->
      if model.pixels <= lowerLimit
      then { model | pixels = upperLimit window }
      else model


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
    , style "width" (String.fromFloat model.pixels ++ "px")
    , style "pointer-events" (if isMoving model then "none" else "auto")
    , style "user-select" (if isMoving model then "none" else "auto")
    , style "transition" (if isMoving model then "none" else "width 0.5s")
    ]


viewRight : Window -> Model -> List (Html msg) -> Html msg
viewRight window model =
  div
    [ id "right-side"
    , style "width" (String.fromFloat (window.width - model.pixels) ++ "px")
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
    , property "pixels" (E.float model.pixels)
    , style "width" (if isTooLarge window model then "40px" else "10px")
    , style "left" (String.fromFloat model.pixels ++ "px")
    ]
    []


decodePixels : D.Decoder Float
decodePixels =
  D.at [ "target", "pixels" ] D.float
