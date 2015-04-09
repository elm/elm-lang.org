import Graphics.Element exposing (..)
import Graphics.Input as Input


main : Signal Element
main =
  Signal.map display check.signal


check : Signal.Mailbox Bool
check =
  Signal.mailbox True


display : Bool -> Element
display checked =
  flow right
    [ container 30 30 middle (Input.checkbox (Signal.message check.address) checked)
    , container 50 30 middle (show checked)
    ]

