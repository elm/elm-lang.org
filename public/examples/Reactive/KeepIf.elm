import Char (isDigit)
import String
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

main : Signal Element
main = let allDigits content = String.all isDigit content.string
       in  display <~ keepIf allDigits Field.noContent numbers.signal

numbers : Input Field.Content
numbers = input Field.noContent

display : Field.Content -> Element
display content =
    Field.field Field.defaultStyle numbers.handle id "Only numbers!" content
