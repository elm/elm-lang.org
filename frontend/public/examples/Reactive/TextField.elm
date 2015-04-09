import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import String


content : Signal.Mailbox Field.Content
content =
  Signal.mailbox Field.noContent


main : Signal Element
main =
  Signal.map scene content.signal


scene : Field.Content -> Element
scene fieldContent =
   flow down
   [ Field.field Field.defaultStyle (Signal.message content.address) "Type here!" fieldContent
   , show (String.reverse fieldContent.string)
   ]
