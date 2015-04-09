
{-------------------------------------------------------------
  Elements can be combined into more complex layouts using
  the flow function:

         flow : Direction -> [Element] -> Element

  It is easy to change the direction of flow. Just use a
  different value for the direction!

     down, up, left, right, inward, outward : Direction

  Try switching "down" in the code below with "up".
-------------------------------------------------------------}

import Graphics.Element exposing (..)


main : Element
main =
  flow down
    [ show "By using the \"flow\" function,"
    , show "we can stack elements"
    , show "on top of other elements."
    ]