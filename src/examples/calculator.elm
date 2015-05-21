import Char
import Color exposing (Color, black, grey, lightGrey, lightOrange, white)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Result
import Signal exposing (Address)
import String
import StartApp


main : Signal Html
main =
  StartApp.start { model = Start zero, view = view, update = update }


-- MODEL

type Model
  = Start Number
  | Operator Float (Float -> Float -> Float) Number


type alias Number =
    { negative : Bool, string : String, percentage : Int }


zero : Number
zero =
  Number False "" 0


stringToNumber : String -> Number
stringToNumber string =
  Number False string 0


numberToFloat : Number -> Float
numberToFloat number =
  let
    neg = if number.negative then -1 else 1
    exp = 100 ^ toFloat number.percentage
  in
    case String.toFloat number.string of
      Ok n -> n * neg / exp
      Err _ -> 0


-- UPDATE

type Action
  = Digit String
  | Decimal
  | Add
  | Subtract
  | Divide
  | Multiply
  | Equals
  | Negate
  | Percentage
  | Clear


update : Action -> Model -> Model
update action model =
  case action of
    Digit digit ->
        let
          isShort number =
            String.length (String.filter Char.isDigit number.string) < 10
        in
          modifyNumber (appendIf isShort digit) model

    Decimal ->
        let
          noDot n = String.all ((/=) '.') n.string
        in
          modifyNumber (appendIf noDot ".") model

    Add ->
        operator (+) model

    Subtract ->
        operator (-) model

    Divide ->
        operator (/) model

    Multiply ->
        operator (*) model

    Equals ->
        Start (stringToNumber (toString (equals model)))

    Negate ->
        modifyNumber (\n -> { n | negative <- not n.negative }) model

    Percentage ->
        modifyNumber (\n -> { n | percentage <- 1 + n.percentage }) model

    Clear ->
        case model of
          Start n ->
              Start zero

          Operator n op m ->
              if m == zero then Start zero else Operator n op zero


modifyNumber : (Number -> Number) -> Model -> Model
modifyNumber f model =
  case model of
    Start n -> Start (f n)
    Operator n op m -> Operator n op (f m)


appendIf : (Number -> Bool) -> String -> Number -> Number
appendIf isOkay str number =
  if isOkay number
    then { number | string <- number.string ++ str }
    else number


operator : (Float -> Float -> Float) -> Model -> Model
operator op model =
  case model of
    Start n ->
        Operator (numberToFloat n) op zero

    Operator n _ m ->
        Operator (if m == zero then n else equals model) op zero


equals : Model -> Float
equals model =
    case model of
      Start n ->
          numberToFloat n

      Operator n op m ->
          op n (if m == zero then n else numberToFloat m)


-- VIEW

view address model =
  div []
    [ div
        [ style
            [ "width" => "120px"
            , "height" => "40px"
            , "background-color" => "black"
            , "color" => "white"
            , "text-overflow" => "ellipsis"
            , "display" => "flex"
            , "align-items" => "center"
            ]
        ]
        [text (viewNumber model)]
    , row
        [ topOp address Clear "C"
        , topOp address Negate "±"
        , topOp address Percentage "%"
        , rightOp address Divide "÷"
        ]
    , numberRow address "1" "2" "3" Multiply "×"
    , numberRow address "4" "5" "6" Subtract "−"
    , numberRow address "7" "8" "9" Add "+"
    , row
        [ lightButton Double address (Digit "0") "0"
        , lightButton Normal address Decimal "."
        , rightOp address Equals "="
        ]
    ]


viewNumber model =
  toString <|
  case model of
    Start n ->
        numberToFloat n

    Operator n op m ->
        if m == zero then n else numberToFloat m



(=>) = (,)


row buttons =
  div
    [ style ["display" => "flex"] ]
    buttons


numberRow address x y z op opName =
  row
    [ number address x
    , number address y
    , number address z
    , rightOp address op opName
    ]


type Width = Normal | Double


btn : Color -> Color -> Width -> Address a -> a -> String -> Html
btn background foreground width address action label =
  let
    widthPx =
      case width of
        Normal -> "30px"
        Double -> "60px"
  in
    div
      [ style
          [ "width" => widthPx
          , "height" => "40px"
          , "background-color" => colorToCss background
          , "color" => colorToCss foreground
          , "display" => "flex"
          , "justify-content" => "center"
          , "align-items" => "center"
          , "cursor" => "pointer"
          ]
      , onClick address action
      ]
      [text label]


lightButton : Width -> Address a -> a -> String -> Html
lightButton =
  btn lightGrey black


number : Address Action -> String -> Html
number address string =
  lightButton Normal address (Digit string) string


topOp : Address a -> a -> String -> Html
topOp =
  btn grey black Normal


rightOp : Address a -> a -> String -> Html
rightOp =
  btn lightOrange white Normal


colorToCss color =
  let
    {red,green,blue,alpha} = Color.toRgb color
  in
    "rgba("
      ++ toString red ++ ", "
      ++ toString green ++ ", "
      ++ toString blue ++ ", "
      ++ toString alpha ++ ")"