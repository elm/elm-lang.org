
import Data.List (intersperse)
import Website.Skeleton

content w = width w [markdown|

### Source Code for this Website

This website was written almost entirely in Elm, so if you want to see
a larger example, you can browse the source code on [github][1]. The
download links are [here][2]. The download also includes almost all of
the examples on this site, which might be nice if you prefer to work on
your own machine.

If you want to see the source code of a particular page in the online editor,
just insert `edit/` after this sites domain name. For instance, 
[elm-lang.org/edit/Examples.elm][3] is the source code for the examples page.

  [1]: https://github.com/evancz/elm-lang.org "github"
  [2]: https://github.com/evancz/elm-lang.org/downloads "download"
  [3]: /edit/Examples.elm "edit Examples.elm"

|]

main = lift (skeleton content) Window.width
