
import Graphics.Input as Input

main : Signal Element
main = lift display check.signal

check : Input.Input Bool
check = Input.input True

display : Bool -> Element
display checked =
  flow right [ container 30 30 middle <| Input.checkbox check.handle identity checked
             , container 50 30 middle <| asText checked
             ]

