import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import TopBar


main =
  div []
    [ TopBar.topBar "examples"
    , Center.markdown "600px" content
    ]


content = """

# Learn by Example

Walk through a sequence of small examples, building skills one at a time by
reading and modifying Elm code in the [online editor](/try).

Remember to check the [syntax reference](/docs/syntax) and [docs](/docs) when
you see new syntax or features!

"""


function =
  [ "apply"
  , "forward-apply"
  , "composition"
  , "anonymous"
  , "infix"
  ]