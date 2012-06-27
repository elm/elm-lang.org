
import Signal.Input (checkbox)

component chkBox checked =
  flow right . map (size 60 30 . box 5) $ [ chkBox, asText checked ]

main = let (box, checked) = checkbox True in
       lift (component box) checked