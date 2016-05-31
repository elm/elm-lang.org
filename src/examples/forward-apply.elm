import Html exposing (text)
import String
{-
  Generally when we want to sort a list and pick a smallest element in the list and repeat the smallest element with String "hello"
  we will have to do the following. 
  disclaimer: In order to find a smallest element, we don't have sort the list. I am using it to strategically explain forward pipe.
-}

smaller_element = String.repeat (convertToInt(List.head (List.sort [8, 7, 9, 2, 5]))) "Hello" --- This prints 2 as a value

{-
  What if we wants to make it readable and simplify this and specificaly without brackets in Elm, how can we do it.
-}

smallest_element = [8, 7, 9, 2, 5] 
  |> List.sort
  |> List.head
  |> convertToInt
  |> flip String.repeat "Hello"

{-
  Those who have worked in Unix environment are pretty familiar with Pipe symbol, this is pretty much it. 
  This line of code allows us read the code from left to right rather than from right to left.

  If you may notice closely, in all functions followed by pipe symbol, the first argument is ignored or not specified explicitly.
-}

main =
  smallest_element
  |> toString
  |> text


-- I had to define this function as List.head returns Maybe function, which needs to be converted to an Int
convertToInt: Maybe number -> Int
convertToInt maybe =
  case maybe of
    Just value -> value
    Nothing -> 0