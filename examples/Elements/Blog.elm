
links = map (text . curry link)
      	[ ("/", "Home"), ("/Examples.elm", "Examples"), ("/docs/docs.elm", "Documentation") ]

blog blogWidth = 
    let content = flowRight
          [ width 100 $ flowDown links
          , width (blogWidth - 200) $ text "this is the actual content."
          , width 100 $ text "ads and other distracting content"
          ]
    in  flowDown . map (centerX . width blogWidth) $
          [ padding 5 . centerText $ header "Example Blog"
          , padding 5 $ image "chessboard.jpg"
          , content
          ]

main = lift1 blog $ lift1 (\w -> if w < 840 then w - 40 else 800) Window.width