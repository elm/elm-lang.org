import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton



main =
  Skeleton.skeleton
    "docs"
    [ Center.markdown "600px" content
    ]


content = """

# Advanced Topics

  * [Extensible Records][ext] &mdash; A full overview of how records work in Elm.
    Gets into details of Daan Leijen's [paper][daan] that first described this
    design.

  * [Taxonomy of FRP][taxonomy] &mdash; A talk that outlines the many flavors
    of FRP. It describes how they work, how they relate to each other, and how
    Elm ended up choosing a point in this design space.

  * [Concurrent FRP][conc] &mdash; Evan&rsquo;s senior thesis on Elm. Includes
    a very accessible history of FRP and overview of how signals work in Elm.

  * [Asynchronous FRP][async] &mdash; The formal semantics of Elm from
    PLDI 2013. This overlaps quite a bit with Concurrent FRP for GUIs but is
    more focused and dryer in tone.


[ext]: /docs/records
[daan]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf
[taxonomy]: https://www.youtube.com/watch?v=Agu6jipKfYw
[conc]: /papers/concurrent-frp.pdf
[async]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html

"""
