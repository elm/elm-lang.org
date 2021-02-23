module TextAnimation exposing
  ( State
  , init
  , step
  , view
  , isMoving
  )


import Cycle



-- STATE


type State
  = Done (Cycle.Cycle String)
  | Typing String Int (Cycle.Cycle String)
  | Deleting String Int (Cycle.Cycle String)


init : String -> List String -> State
init x xs =
  Deleting "" 0 (Cycle.init x xs)


step : State -> State
step state =
  case state of
    Done cycle ->
      Deleting (Cycle.next cycle) 0 (Cycle.step cycle)

    Typing letters count cycle ->
      if count < 4 then
        Typing letters (count + 1) cycle
      else
        let
          next = Cycle.next cycle
          len1 = String.length letters
          len2 = String.length next
        in
        if len1 == len2
        then Done cycle
        else Typing (String.left (len1 + 1) next) 0 cycle

    Deleting letters count cycle ->
      if count < 1 then
        Deleting letters (count + 1) cycle
      else
        let
          len = String.length letters
        in
        if len == 0
        then Typing "" 0 cycle
        else Deleting (String.left (len - 1) letters) 0 cycle


view : State -> String
view state =
  case state of
    Done cycle     -> Cycle.next cycle
    Typing s _ _   -> s
    Deleting s _ _ -> s


isMoving : State -> Bool
isMoving state =
  case state of
    Done _         -> False
    Typing _ _ _   -> True
    Deleting _ _ _ -> True
