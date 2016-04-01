import Html exposing (text)


main =
  text (toString (quicksort [5,3,8,1,9,4,7]))


quicksort : List comparable -> List comparable
quicksort list =
  case list of
    [] ->
        []

    pivot :: rest ->
        let
          lower  = List.filter (\n -> n <= pivot) rest
          higher = List.filter (\n -> n >  pivot) rest
        in
          quicksort lower ++ [pivot] ++ quicksort higher


{---------------------

QuickSort works as follows:

 - Choose a pivot element to put in the "middle" of the sorted list.
 - Gather all of the elements less than the pivot in `lower`.
 - Gather all of the elements greater than the pivot in `higher`.
 - Run `quicksort` on the `lower` and `higher` lists, sorting them.
 - Put the sorted lists together.


Note that choosing a bad pivot can have bad effects. Take a sorted
list with N elements. The pivot will always be the lowest member,
meaning that it does not divide the list very evenly. The list of
lessers has 0 elements and the list of greaters has N-1 elemens.
This means quicksort will be called N times, each call looking
through the entire list. This means, in the worst case, QuickSort
will make N^2 comparisons.

----------------------}
