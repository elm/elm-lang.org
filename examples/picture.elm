-- Create pictures from simple shapes. Like a tree!
--
-- Learn more about the playground here:
--   https://package.elm-lang.org/packages/evancz/elm-playground/latest/
--

import Playground exposing (..)


main =
  picture
    [ rectangle brown 40 200
        |> moveDown 80
    , circle green 100
        |> moveUp 100
    ]
