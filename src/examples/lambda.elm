import Html exposing (text)
import String


{- Usually we name all the functions we use. So the following
function is named `increment` and we can refer to it by that name
anywhere in this file.
-}
increment n =
  n + 1


{- But no one is forcing us to name every function. Instead of saying:

    List.map increment [1,2,3] == [2,3,4]

We could skip defining `increment` separately and just say this:

    List.map (\n -> n + 1) [1,2,3] == [2,3,4]

Here we are using an anonymous function. These are sometimes called
"lambdas" for historical reasons, and the starting backslash is kind
of a pun on that history: a \ looks a bit like λ if you squint!

If you are coming from JavaScript, saying (\n -> n + 1) is just like
saying (function(n) { return n + 1; })

That means we can define `increment` another way:
-}
add1 =
  \n -> n + 1


{- Defining a function and naming an anonymous function are the same thing!
It just looks prettier to define a function the "normal" way.
-}
main =
  text (toString (increment (add1 40)))