
{----------------------------------------------------------
  The "box" function creates a box around an element. It
  also allows you to position the element within the box.
  There are nine possible positions specified by an Int:

             box :: Int -> Element -> Element

  The possible positions     +---+---+---+
  are given in the table     | 1 | 2 | 3 |
  to the right.              +---+---+---+
                             | 4 | 5 | 6 |
  Remember the positions     +---+---+---+
  by thinking of dialing     | 7 | 8 | 9 |
  a phone.                   +---+---+---+

----------------------------------------------------------}

import Signal.Window (dimensions)

scene (w,h) = size w h . box 5 $ plainText "Hello, World!"

main = lift scene dimensions

-- Try changing the size of your browser window.