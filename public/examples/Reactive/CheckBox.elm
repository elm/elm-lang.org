
import Graphics.Input as Input

main = lift display checked

(checked, portal) = Input.input True

display checked =
  flow right [ container 30 30 middle <| Input.checkbox portal id checked
             , container 50 30 middle <| asText checked
             ]

