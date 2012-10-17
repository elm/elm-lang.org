
import Input (checkbox)

component chkBox checked =
  container 30 30 middle chkBox `beside` container 40 30 midLeft (asText checked)

main = let (box, checked) = checkbox True in
       lift (component box) checked