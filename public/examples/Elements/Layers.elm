
main = layers
  [ fittedImage 320 240 "/shells.jpg"
  , width 320 . centered . header <| toText "She sells sea shells."
  ]
