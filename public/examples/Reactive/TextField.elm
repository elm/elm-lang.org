
import Graphics.Input as Input

(field, content) = Input.field "Type here!"

main = lift2 above field (lift (plainText . reverse) content)
