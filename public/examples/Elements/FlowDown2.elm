
content = [ plainText "Bears, Oh My!"
          , video 320 240 "/bear.ogg"
          , image 472 315 "/shells.jpg"
          ]

main = flow down (map (width 150) content)