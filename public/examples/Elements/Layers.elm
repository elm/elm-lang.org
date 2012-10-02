
main = layers
  [ video 320 240 "/bear.ogg"
  , width 320 . centeredText . header $ toText "Birds and Bears"
  ]
