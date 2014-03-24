import Char (isDigit)
import String (all)
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

main : Signal Element
main = display <~ keepIf (all isDigit . .string) Field.noContent numbers.signal

numbers : Input Field.Content
numbers = input Field.noContent

display : Field.Content -> Element
display content =
    Field.field Field.defaultStyle numbers.handle id "Only numbers!" content
