
main =
  collage 200 200
  [ rotate (degrees 45) . toForm <| plainText "Any element can go here!" ]