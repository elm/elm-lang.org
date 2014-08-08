import Mouse
import Text
import Window

toDegrees : Int -> Float
toDegrees i = degrees <| toFloat <| mod i 360

title : String -> Element
title s = 
  let titleStyle = 
    { typeface = [ "Helvetica Neue" ],
      height = Just 36,
      color = black,
      bold = False,
      italic = False,
      line = Nothing
    } 
  in
  centered <| style titleStyle <| toText s

linkStyle : Style
linkStyle =
  { typeface = [ "Helvetica Neue" ],
    height = Just 14, 
    color = black,
    bold = False,
    italic = False,
    line = Nothing
  }
 
blog : Text
blog = style linkStyle <| Text.link "http://blog.jameslarisch.com" <| toText "blog"

github : Text
github = style linkStyle <| Text.link "http://github.com/semaj" <| toText "github"

linkedin : Text
linkedin = style linkStyle <| Text.link "http://linkedin.com/in/jameslarisch/" <| toText "linkedin"

links : Element
links = centered <| blog ++ (toText " | ") ++ github ++ (toText " | ") ++ linkedin


getSpinners : Int -> Int -> Int -> Form
getSpinners x y c =
  group [
    rotate (toDegrees x) (filled blue (ngon 5 40)),
    move (0, 0) <| rotate (toDegrees c) (filled green (square 40)),
    move (0, 0) <| rotate (toDegrees y) (filled orange (ngon 3 20))
  ]


getPage : (Int, Int) -> Int -> (Int,Int) -> Element
getPage (x, y) c (w,h) = 
  flow down [
    width w <| title "James Larisch",
    width w <| links,
    collage w h [
      toForm <| plainText <| show (round (0.9 * (toFloat h))),
      move (0, (0.43 * (toFloat h))) <| getSpinners x y c
    ]
  ]


main = lift3 getPage Mouse.position (count Mouse.position) Window.dimensions

body : Text
body = 
  asText "Hi! Weird website huh? It's " ++  
  Text.link written purely in " ++ 
  Text.link "http://elm-lang.org/" (asText "Elm") ++
  asText ", a functional-reactive language that compiles down to CSS/HTML/JS.\n\n" ++
  asText "As for me, I'm a Software Engineer.\n\n" ++
  asText "



