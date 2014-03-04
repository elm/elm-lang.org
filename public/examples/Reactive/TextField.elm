import String
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

content : Input Field.Content
content = input Field.noContent

main : Signal Element
main = lift2 above
         (Field.field content.handle id Field.defaultStyle "Type here!" <~ content.signal)
         (plainText . String.reverse . .string <~ content.signal)
