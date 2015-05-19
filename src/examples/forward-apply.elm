import Color exposing (green, purple)
import Graphics.Collage exposing (collage, filled, move, ngon)


main =
  collage 200 200 [purplePentagon, greenPentagon]


{-| We use three different functions to define our purple pentagon.

  1. 'move' takes a pair of coordinates and a textured shape
  2. 'filled' takes a color and a shape
  3. 'ngon' takes a number of corners and a radius

We need to put all of these together, so we end up with a decent
number of parentheses.
-}
purplePentagon =
  move (20,20) (filled purple (ngon 5 50))


{-| We are going to use the same three functions to define a green
pentagon, but this time we will use the |> operator to use fewer
parentheses. It is defined like this:

    x |> f = f x

So as soon as we evaluate 'x' we hand it to the function 'f' and
keep going. So in the following code, we create an ngon, fill it
in, and move it around.

This is pretty much the same as the purplePentagon code, we just
traded parentheses for the "forward application" operator. It also
means the code reads left-to-right, often making it easier to read!
-}
greenPentagon =
  ngon 5 50
    |> filled green
    |> move (-20,-20)


-- EXERCISE: try switching greenPentagon to only use parentheses, and
-- try switching purplePentagon to only use the |> operator.