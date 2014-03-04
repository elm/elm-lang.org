
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

main : Signal Element
main = lift display password.signal

password : Input Field.Content
password = input Field.noContent

display : Field.Content -> Element
display content =
  flow down [ Field.password password.handle id Field.defaultStyle "Password" content
            , plainText ("Your password is: " ++ content.string)
            ]