
outline =  line [ (50,50), (150,50), (150,150), (50,150), (50,50) ]

main = collage 200 530
         [ dashed blue outline
         , move 0 110 $ dotted green outline
         , move 0 220 $ solid  red   outline
         , move 0 330 $ customLine [8,4,2,4] black outline
         ]