import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import Signal
import String
import Text exposing (plainText)


content : Signal.Channel Field.Content
content =
  Signal.channel Field.noContent


main : Signal Element
main =
  Signal.map scene (Signal.subscribe content)


scene : Field.Content -> Element
scene fieldContent =
   flow down
   [ Field.field Field.defaultStyle (Signal.send content) "Type here!" fieldContent
   , plainText (String.reverse fieldContent.string)
   ]
