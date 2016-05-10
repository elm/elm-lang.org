import Html exposing (text)


{-| This is a union type that lets you put together two types.
If you tag a value with 'Left' it can have type 'a' and if you
tag a value with 'Right' it can have type 'b'.
-}
type Either a b
    = Left a
    | Right b


{-| Maybe you have a list of user IDs and those IDs are either
an integer or a string depending on when the user joined.
-}
userIDs : List (Either Int String)
userIDs =
  [Left 42, Right "12A3BC", Left 1337, Right "ZA7T9G"]


{-| Starting with a list of eithers, partition them into a list
of left values and a list of right values.
-}
partition : List (Either a b) -> (List a, List b)
partition eithers =
  case eithers of
    [] ->
      ([], [])

    Left a :: rest ->
      let
        (lefts, rights) =
          partition rest
      in
        (a :: lefts, rights)

    Right b :: rest ->
      let
        (lefts, rights) =
          partition rest
      in
        (lefts, b :: rights)


main =
  text (toString (partition userIDs))