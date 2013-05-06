
import Graphics.Input as Input

display field state =
  field `above` asText state

main = let (field, state) = Input.field "Type here!"
       in  lift2 display field state

