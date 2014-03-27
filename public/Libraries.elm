import Website.Button as B
import Website.Skeleton (skeleton)
import String
import Window

main = lift (skeleton content) Window.dimensions

content w =
  flow down
  [ width w intro
  , B.button w 400 "http://library.elm-lang.org/catalog/evancz-Elm/0.12/" "Standard Libraries"
  , width w midtro
  , B.button w 400 "http://library.elm-lang.org/catalog" "Community Libraries"
  , width w outro
  ]

intro = [markdown|

# Libraries

The Standard Libraries come with the latest release of the Elm compiler and
make it easy to get productive. When you need documentation for functions like
`map` and `length` or operators like `/=` and `|>`, just **filter** to find out
which module it is in:

|]

midtro = [markdown|

If you cannot find it in the Standard Libraries, the Elm community is probably
working on it already. Browse community libraries and check out their documentation:

|]

outro = [markdown|

See the [syntax reference](/learn/Syntax.elm) and [other learning
resources](/Learn.elm) to learn more about the language itself.

|]