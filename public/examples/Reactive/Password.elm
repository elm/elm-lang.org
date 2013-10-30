
import Graphics.Input as Input

main = let (field, password) = Input.password "Password"
       in  lift2 display field password

display field password =
  field `above` plainText ("Your password is: " ++ password)