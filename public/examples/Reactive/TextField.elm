import String
import Graphics.Input (Input, input, FieldContent, noContent, field)

content : Input FieldContent
content = input noContent

main : Signal Element
main = lift2 above
         (field content.handle id "Type here!" <~ content.signal)
         (plainText . String.reverse . .string <~ content.signal)
