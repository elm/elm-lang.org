
{----------------------------------------------------------------

Function composition passes results along in the suggested
direction. For example, the following code checks if the square
of a number is odd:

      square >> isEven >> not

You can think of this operator as equivalent to the following:

      (f >> g)  ==  (\x -> g (f x))

So our example expands out to something like this:

      \n -> not (isEven (square n))

----------------------------------------------------------------}

import Graphics.Element exposing (show)


-- simple functions

square : Int -> Int
square n =
    n * n


isEven : Int -> Bool
isEven n =
    n % 2 == 0


-- composed functions

squareIsOdd : Int -> Bool
squareIsOdd =
    square >> isEven >> not


main = show (squareIsOdd 3)