module Data.Fetched exposing
  ( Fetched(..)
  , map, map2, withDefault
  )

import Http


type Fetched a
  = Loading
  | Success a
  | Failed Http.Error


map : (a -> b) -> Fetched a -> Fetched b
map func ma =
  case ma of
    Success a  -> Success (func a)
    Loading    -> Loading
    Failed err -> Failed err


map2 : (a -> b -> c) -> Fetched a -> Fetched b -> Fetched c
map2 func ma mb =
  case ( ma, mb) of
    ( Success a, Success b )  -> Success (func a b)
    ( Success a, Loading )    -> Loading
    ( Success a, Failed err ) -> Failed err
    ( Loading, Success b )    -> Loading
    ( Loading, Loading )      -> Loading
    ( Loading, Failed err )   -> Failed err
    ( Failed err, Success b ) -> Failed err
    ( Failed err, Loading )   -> Failed err
    ( Failed err, Failed _ )  -> Failed err


withDefault : a -> Fetched a -> a
withDefault default ma =
  case ma of
    Loading   -> default
    Success a -> a
    Failed _  -> default
