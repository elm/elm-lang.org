import Graphics.Element exposing (..)
import Graphics.Input.Field as Field


main : Signal Element
main =
  Signal.map display password.signal


password : Signal.Mailbox Field.Content
password =
  Signal.mailbox Field.noContent


display : Field.Content -> Element
display content =
  flow down
    [ Field.password Field.defaultStyle (Signal.message password.address) "Password" content
    , show ("Your password is: " ++ content.string)
    ]