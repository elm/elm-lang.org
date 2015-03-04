import Graphics.Element exposing (..)
import Graphics.Input as Input
import Text exposing (asText)


main : Varying Element
main =
  Varying.map display (Signal.subscribe check)


check : Signal.Channel Bool
check =
  Signal.channel True


display : Bool -> Element
display checked =
  flow right
    [ container 30 30 middle (Input.checkbox (Signal.send check) checked)
    , container 50 30 middle (asText checked)
    ]

