import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import JavaScript.Decode as JS exposing ((:=))
import Port
import String
import Task exposing (..)


-- VIEW

view : String -> Result String (List String) -> Html
view string result =
  let field =
        input
          [ placeholder "Zip Code"
          , value string
          , on "input" targetValue (Port.message query.address)
          , myStyle
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div [ myStyle ] [ text msg ] ]

          Ok cities ->
              List.map (\city -> div [ myStyle ] [ text city ]) cities
  in
      div [] (field :: messages)


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- WIRING

main =
  Varying.map2 view
    (Varying.fromStream "" query.stream)
    (Varying.fromStream (Err "A valid US zip code is 5 numbers.") results.stream)


port query : Port.Port String

port results : Port.Port (Result String (List String))

perform
  Task.subscribe (Stream.map lookupZipCode query.stream) <| \task ->
    (task `andThen` (Ok >> Port.send results.address))
      `onError` (Err >> Port.send results.address)


lookupZipCode : String -> Task String (List String)
lookupZipCode query =
  let toUrl =
        if String.length query == 5 && String.all Char.isDigit query
          then succeed ("http://api.zippopotam.us/us/" ++ query)
          else fail "Give me a valid US zip code!"
  in
      toUrl `andThen` (mapError (always "Not found :(") << Http.get places)


places : JS.Decoder (List String)
places =
  let place =
        JS.object2 (\city state -> city ++ ", " ++ state)
          ("place name" := JS.string)
          ("state" := JS.string)
  in
      "places" := JS.list place