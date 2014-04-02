
{-------------------------------------------------------------
  Elements can be combined into more complex layouts using
  the flow function:

         flow : Direction -> [Element] -> Element

  It is easy to change the direction of flow. Just use a
  different value for the direction!

     down, up, left, right, inward, outward : Direction

  Try switching "down" in the code below with "up".
-------------------------------------------------------------}

main = flow down [ plainText "By using the \"flow\" function,",
                   plainText "we can stack elements",
                   plainText "on top of other elements." ]