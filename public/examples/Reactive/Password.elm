
import Graphics.Input (Input, input, FieldContent, noContent, password)

main : Signal Element
main = lift display content.signal

content : Input FieldContent
content = input noContent

display fieldContent =
  flow down [ password content.handle id "Password" fieldContent
            , plainText ("Your password is: " ++ fieldContent.string)
            ]