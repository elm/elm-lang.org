import String
import Graphics.Input as Input

(content, portal) = Input.input Input.noContent

main = lift2 above
         (Input.field portal id "Type here!" <~ content)
         (plainText . String.reverse . .string <~ content)
