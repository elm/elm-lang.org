
main : Element
main =
    collage 200 420
        [ move (0,-55) blueSquare
        , move (0, 55) redSquare
        ]

blueSquare : Form
blueSquare = traced (dashed blue) square

redSquare : Form
redSquare = traced (solid red) square

square : Path
square = path [ (50,50), (50,-50), (-50,-50), (-50,50), (50,50) ]