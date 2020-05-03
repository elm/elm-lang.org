module Cycle exposing
  ( Cycle
  , init
  , next
  , step
  )


type Cycle a =
  Cycle a (List a)


init : a -> List a -> Cycle a
init x xs =
  Cycle x xs


next : Cycle a -> a
next (Cycle x _) =
  x


step : Cycle a -> Cycle a
step (Cycle xold xs) =
  case xs of
    [] ->
      Cycle xold []
    
    xnew::xother ->
      Cycle xnew (xother ++ [xold])