
content = [ plainText "Bears, Oh My!"
          , video "/bear.ogg"
          , image "/yogi.jpg"
          ]

main = flow down (List.map (width 150) content)