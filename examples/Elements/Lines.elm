
outline =  line [ (50,50), (150,50), (150,150), (50,150), (50,50) ]

main = collage 200 530
         [ dashed blue outline
         , dotted green $ move 0 110 outline
         , solid  red   $ move 0 220 outline
         , customLine [8,4,2,4] black $ move 0 330 outline
         ]