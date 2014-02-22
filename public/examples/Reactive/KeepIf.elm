import Char (isDigit)
import String (all)
import Graphics.Input (Input, input, FieldContent, noContent, field)

main : Signal Element
main = scene <~ keepIf (\c -> all isDigit c.string) noContent content.signal

content : Input FieldContent
content = input noContent

scene : FieldContent -> Element
scene fieldContent =
    field content.handle id "Only numbers!" fieldContent
