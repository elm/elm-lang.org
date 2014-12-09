import Color (..)
import Graphics.Element (..)
import Graphics.Input.Field as Field
import Graphics.Input as Input
import Http
import List
import Signal
import String
import Text
import Window


-- MODEL

type alias Model =
    { first : Field.Content
    , last : Field.Content
    , email : Field.Content
    , remail : Field.Content
    , sendAttempts : Int
    }


emptyModel : Model
emptyModel =
    { first = Field.noContent
    , last = Field.noContent
    , email = Field.noContent
    , remail = Field.noContent
    , sendAttempts = 0
    }


-- UPDATE

type Update
    = First Field.Content
    | Last Field.Content
    | Email Field.Content
    | Remail Field.Content
    | Submit


update : Update -> Model -> Model
update updt model =
  case updt of
    First content ->
        { model | first <- content }

    Last content ->
        { model | last <- content }

    Email content ->
        { model | email <- content }

    Remail content ->
        { model | remail <- content }

    Submit ->
        { model | sendAttempts <- model.sendAttempts + 1 }


getErrors : Model -> List String
getErrors {first,last,email,remail} =
  let isEmpty content =
        String.isEmpty content.string

      checks =
        [ (isEmpty first , "First name required.")
        , (isEmpty last  , "Last name required.")
        , (isEmpty email , "Must enter your email address.")
        , (isEmpty remail, "Must re-enter your email address.")
        , (email.string /= remail.string, "Email addresses do not match.")
        ]

      activeError (err,msg) =
        if err then Just msg else Nothing
  in
      List.filterMap activeError checks


-- VIEW

view : (Int,Int) -> Model -> Element
view (w,h) model =
  color charcoal <|
    flow down
    [ spacer w 50
    , container w (h-50) midTop (viewForm model)
    ]


header : Element
header =
  Text.fromString "Example Sign Up"
    |> Text.height 32
    |> Text.leftAligned


viewForm : Model -> Element
viewForm model =
    color lightGrey <|
      flow down
      [ container 360 60 middle header
      , viewField "First Name:" model.first First
      , viewField "Last Name:" model.last Last
      , viewField "Your Email:" model.email Email
      , viewField "Re-enter Email:" model.remail Remail
      , viewErrors model
      , container 300 50 midRight <|
          size 60 30 <|
            Input.button (Signal.send updateChan Submit) "Submit"
      ]


viewField : String -> Field.Content -> (Field.Content -> Update) -> Element
viewField label content toUpdate =
  flow right
    [ container 140 36 midRight (Text.plainText label)
    , container 220 36 middle <|
        size 180 26 <|
          Field.field Field.defaultStyle (Signal.send updateChan << toUpdate) "" content
    ]


viewErrors : Model -> Element
viewErrors model =
  let errors =
        if model.sendAttempts > 0 then getErrors model else []
  in
      flow down
        [ spacer 10 10
        , if List.isEmpty errors
            then spacer 0 0
            else flow down (List.map viewError errors ++ [spacer 10 10])
        ]


viewError : String -> Element
viewError msg =
  Text.fromString msg
    |> Text.color red
    |> Text.centered
    |> width 360


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions model


model : Signal Model
model =
  Signal.subscribe updateChan
    |> Signal.foldp update emptyModel


updateChan : Signal.Channel Update
updateChan =
  Signal.channel Submit


port redirect : Signal String
port redirect =
    Signal.map2 toUrl (Signal.subscribe updateChan) model
      |> Signal.keepIf (not << String.isEmpty) ""


toUrl : Update -> Model -> String
toUrl update model =
  if not (List.isEmpty (getErrors model))
    then ""
    else
      case update of
        Submit ->
          "/login?first=" ++ model.first.string
          ++ "&last=" ++ model.last.string
          ++ "&email=" ++ model.email.string

        _ -> ""

