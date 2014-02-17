
import Graphics.Input as Input

main = lift display content

(content, portal) = Input.input Input.noContent

display content =
  flow down [ Input.password portal id "Password" content
            , plainText ("Your password is: " ++ content.string)
            ]