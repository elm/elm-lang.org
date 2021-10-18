module Data.Window exposing (Window, isMobile, isLessThan)


type alias Window =
  { width : Int
  , height : Int
  }


isMobile : Window -> Bool
isMobile window =
  isLessThan window 1000


isLessThan : Window -> Int -> Bool
isLessThan window num =
  window.width <= num