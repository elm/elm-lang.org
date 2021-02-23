module Cycle exposing
  ( Cycle
  , init
  , next
  , step
  , toList
  , select
  )


type Cycle a =
  Cycle (List a) a (List a)


init : a -> List a -> Cycle a
init x xs =
  Cycle [] x xs


next : Cycle a -> a
next (Cycle _ x _) =
  x


step : Cycle a -> Cycle a
step (Cycle visited a unvisited) =
  case unvisited of
    [] ->
      restart visited a []

    x :: xs ->
      Cycle (a :: visited) x xs


restart : List a -> a -> List a -> Cycle a
restart visited a unvisited =
  case visited of
    [] ->
      Cycle [] a unvisited

    x :: xs ->
      restart xs x (a :: unvisited)


toList : Cycle a -> List a
toList (Cycle visited a unvisited) =
  List.reverse visited ++ [ a ] ++ unvisited


select : Int -> Cycle a -> Cycle a
select index cycle =
  let list = toList cycle in
  case ( List.take index list, List.drop index list ) of
    ( visited, selected :: unvisited ) -> Cycle (List.reverse visited) selected unvisited
    ( visited, unvisited ) -> cycle




