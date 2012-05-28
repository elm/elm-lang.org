
data Color = Red | Green | Blue | Orange | Color Int Int Int

options = [ ("Red"  , Red)
          , ("Green", Green)
          , ("Black", Color 0 0 0)
          ]

main = let (selector, color) = select options in
       flowDown [ flowRight [ text "Set text color to ", selector ]
                , text $ textColor color "Hello, World!"
                ]