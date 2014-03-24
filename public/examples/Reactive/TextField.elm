import String
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

content : Input Field.Content
content = input Field.noContent

main : Signal Element
main = lift2 above
         (Field.field Field.defaultStyle content.handle id "Type here!" <~ content.signal)
         (plainText . String.reverse . .string <~ content.signal)
