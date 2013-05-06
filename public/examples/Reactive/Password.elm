
import Graphics.Input as Input

display field state =
  field `above` (plainText <| "Your password is: " ++ state.string)

main = let (field, state) = Input.password "Password"
       in  lift2 display field state