
component chkBox checked =
  flow right . List.map (size 60 30 . box 5) $ [ chkBox, asText checked ]

main = case Input.checkbox True of
       { (box, checked) -> lift (component box) checked }