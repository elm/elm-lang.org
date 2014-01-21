
main = layers
  [ fittedImage 320 240 "/shells.jpg"
  , width 320 . centered <| toText "She sells sea shells."
  ]
