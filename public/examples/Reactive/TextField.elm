
import Graphics.Input as Input

main = let (field, state) = Input.field "Type here!"
       in  lift2 display field state

display field state =
  field `above` asText state
