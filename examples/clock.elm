-- elm install elm/svg


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = toFloat (Time.toHour   model.zone model.time)
    minute = toFloat (Time.toMinute model.zone model.time)
    second = toFloat (Time.toSecond model.zone model.time)
  in
  svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    [ circle [ cx "200", cy "200", r "120", fill "#1293D8" ] []
    , viewHand 6 60 (hour/12)
    , viewHand 6 90 (minute/60)
    , viewHand 3 90 (second/60)
    ]


viewHand : Int -> Float -> Float -> Svg msg
viewHand width length turns =
  let
    t = 2 * pi * (turns - 0.25)
    x = 200 + length * cos t
    y = 200 + length * sin t
  in
  line
    [ x1 "200"
    , y1 "200"
    , x2 (String.fromFloat x)
    , y2 (String.fromFloat y)
    , stroke "white"
    , strokeWidth (String.fromInt width)
    , strokeLinecap "round"
    ]
    []
