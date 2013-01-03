
component chkBox checked =
  container 30 30 middle chkBox `beside` container 40 30 midLeft (asText checked)

(box, checked) = Input.checkbox True

main = component box <~ checked