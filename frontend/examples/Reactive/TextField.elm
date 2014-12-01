import String
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

content : Input Field.Content
content = input Field.noContent

main : Signal Element
main =
    lift scene content.signal

scene : Field.Content -> Element
scene fieldContent =
   flow down
   [ Field.field Field.defaultStyle content.handle identity "Type here!" fieldContent
   , plainText (String.reverse fieldContent.string)
   ]
