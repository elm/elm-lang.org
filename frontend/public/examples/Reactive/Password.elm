import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import Signal
import Text exposing (plainText)


main : Signal Element
main =
  Signal.map display (Signal.subscribe password)


password : Signal.Channel Field.Content
password =
  Signal.channel Field.noContent


display : Field.Content -> Element
display content =
  flow down
    [ Field.password Field.defaultStyle (Signal.send password) "Password" content
    , plainText ("Your password is: " ++ content.string)
    ]