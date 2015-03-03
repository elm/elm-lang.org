import Graphics.Element exposing (..)
import Graphics.Input as Input
import Signal
import Text exposing (asText)


main : Signal Element
main =
  Signal.map display (Signal.subscribe check)


check : Signal.Channel Bool
check =
  Signal.channel True


display : Bool -> Element
display checked =
  flow right
    [ container 30 30 middle (Input.checkbox (Signal.send check) checked)
    , container 50 30 middle (asText checked)
    ]

