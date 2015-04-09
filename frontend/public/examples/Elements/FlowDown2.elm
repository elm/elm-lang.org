import Graphics.Element exposing (..)


main : Element
main =
  flow down (List.map (width 150) content)


content : List Element
content =
  [ show "Bears, Oh My!"
  , image 200 200 "/yogi.jpg"
  , image 472 315 "/shells.jpg"
  ]
