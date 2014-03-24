import Char
import String
import Graphics.Input as Input
import Window

main : Signal Element
main = calculator <~ Window.dimensions ~ foldp step (Start zero) commands.signal


-- INPUTS

commands : Input.Input Command
commands = Input.input Clear


-- MODEL

data Command
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

data State
    = Start Number
    | Operator Float (Float -> Float -> Float) Number

type Number = { negative : Bool, string : String, percentage : Int }

mkNumber : String -> Number
mkNumber n = { negative = False, string = n, percentage = 0 }

zero : Number
zero = { negative = False, string = "", percentage = 0 }

numberToFloat : Number -> Float
numberToFloat number =
    let neg = if number.negative then -1 else 1
        exp = 100 ^ toFloat number.percentage
    in  maybe 0 id (String.toFloat number.string) * neg / exp


-- DISPLAY

buttonSize : number
buttonSize = 80

calculator : (Int,Int) -> State -> Element
calculator (w,h) state =
    let pos = bottomRightAt (absolute 10) (absolute 10)
    in  color darkCharcoal . container w h middle <|
        flow down [ color black . container (4*buttonSize) (buttonSize+40) pos <|
                    screen 0.6 (show (displayNumber state))
                  , buttons
                  ]

screen : Float -> String -> Element
screen size text =
  let number = txt size white text in
  if widthOf number > 4*buttonSize - 20
      then screen (size-0.02) text
      else number

displayNumber : State -> Float
displayNumber state =
    case state of
      Start n -> numberToFloat n
      Operator n op m -> if m == zero then n else numberToFloat m

buttons : Element
buttons =
    flow down
    [ flow right [ topOp Clear "C", topOp Negate "&plusmn;"
                 , topOp Percentage "%", rightOp Divide "&divide;" ]
    , flow right [ number "1", number "2", number "3", rightOp Multiply "&times;" ]
    , flow right [ number "4", number "5", number "6", rightOp Subtract "&minus;" ]
    , flow right [ number "7", number "8", number "9", rightOp Add "+" ]
    , flow right [ lightButton (2*buttonSize) buttonSize (Digit "0") "0"
                 , lightButton buttonSize buttonSize Decimal "."
                 , rightOp Equals "=" ]
    ]

button : Color -> Color -> Int -> Int -> Command -> String -> Element
button background foreground w h command name =
    let n = min w h
        btn alpha =
            layers [ color black . container w h bottomRight .
                     color background . container (w-1) (h-1) midLeft .
                     container n n middle <| txt 0.3 foreground name
                   , color (rgba 0 0 0 alpha) (spacer w h)
                   ]
    in  Input.customButton commands.handle command (btn 0) (btn 0.05) (btn 0.1)

lightButton : Int -> Int -> Command -> String -> Element
lightButton = button lightGrey black

number : String -> Element
number n =
    lightButton buttonSize buttonSize (Digit n) n

topOp : Command -> String -> Element
topOp command name =
    button grey black buttonSize buttonSize command name

rightOp : Command -> String -> Element
rightOp command name =
    button lightOrange white buttonSize buttonSize command name

txt : Float -> Color -> String -> Element
txt p clr string =
    leftAligned . Text.height (p * buttonSize) .
    typeface ["Helvetica Neue","Sans-serif"] . Text.color clr <| toText string


-- UPDATE

step : Command -> State -> State
step command state =
    case command of
      Digit digit ->
          let isShort n = String.length (String.filter Char.isDigit n.string) < 10
          in  modifyNumber (appendIf isShort digit) state

      Decimal ->
          let noDot n = String.all ((/=) '.') n.string
          in  modifyNumber (appendIf noDot ".") state

      Add -> operator (+) state
      Subtract -> operator (-) state
      Divide -> operator (/) state
      Multiply -> operator (*) state
      Equals -> Start (mkNumber (show (equals state)))
      Negate -> modifyNumber (\n -> { n | negative <- not n.negative }) state
      Percentage -> modifyNumber (\n -> { n | percentage <- 1 + n.percentage }) state
      Clear -> clear state

modifyNumber : (Number -> Number) -> State -> State
modifyNumber f state =
    case state of
      Start n -> Start (f n)
      Operator n op m -> Operator n op (f m)

appendIf : (Number -> Bool) -> String -> Number -> Number
appendIf isOkay str number =
    if isOkay number
    then { number | string <- number.string ++ str }
    else number

clear : State -> State
clear state =
    case state of
      Start n -> Start zero
      Operator n op m ->
          if m == zero then Start zero else Operator n op zero

operator : (Float -> Float -> Float) -> State -> State
operator op state =
    case state of
      Start n -> Operator (numberToFloat n) op zero
      Operator n _ m ->
          Operator (if m == zero then n else equals state) op zero

equals : State -> Float
equals state =
    case state of
      Start n -> numberToFloat n
      Operator n op m -> op n (if m == zero then n else numberToFloat m)