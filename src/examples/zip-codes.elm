import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Task
import String
import Char
import Http
import StartApp


-- MODEL

type alias Model =
    { code : String
    , cities : List String
    }


init : (Model, Effects Action)
init =
    ( Model "" ["Give me a valid US zip code!"]
    , Effects.none
    )


-- UPDATE

type Action
    = NewCode String
    | Update (Maybe (List String))


update : Action -> Model -> (Model, Effects Action)
update message model =
    case message of
        NewCode code ->
            ( { model | code <- code }
            , getPlace code
            )

        Update maybeCities -> 
            case maybeCities of 
              Just cities -> ( { model | cities <- cities }, Effects.none)
              Nothing -> ( { model | cities <- ["A valid US zip code is 5 numbers."] }, Effects.none)


getPlace : String -> Effects Action
getPlace code =
  if String.length code == 5 && String.all Char.isDigit code
    then 
      Http.get places ("http://api.zippopotam.us/us/" ++ code)
      |> Task.toMaybe
      |> Task.map Update
      |> Effects.task
    else 
      Task.succeed (Update Nothing) 
      |> Effects.task 


places : Json.Decoder (List String)
places =
  let place =
        Json.object2 (\city state -> city ++ ", " ++ state)
          ("place name" := Json.string)
          ("state" := Json.string)
  in
      "places" := Json.list place


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let 
    textField =
      input
        [ placeholder "Zip Code"
        , value model.code
        , on "input" targetValue (Signal.message address << NewCode)
        , myStyle
        ]
        [] 

    citiesDivs = List.map (\city -> div [ myStyle ] [ text city ]) model.cities

  in 
    div [] (textField :: citiesDivs) 


myStyle : Attribute
myStyle =
    style
        [ ("width", "100%")
        , ("height", "40px")
        , ("padding", "10px 0")
        , ("font-size", "2em")
        , ("text-align", "center")
        ]


-- APP CONFIG

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
