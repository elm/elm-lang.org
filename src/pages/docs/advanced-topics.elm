import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton


(=>) = (,)


main =
  Skeleton.skeleton
    "docs"
    [ div [style
            [("max-width", "600px"), ("margin", "0 auto")]
          ]
      [Center.markdown "600px" beginning
      , div [ class "intrinsic-container" ]
          [ iframe
              [ src "https://www.youtube.com/embed/DfLvDFxcAIA"
              , attribute "allowfullscreen" ""
              ]
              []
          ]
      , Center.markdown "600px" middle
      , div [ class "intrinsic-container" ]
          [ iframe
              [ src "https://www.youtube.com/embed/Agu6jipKfYw"
              , attribute "allowfullscreen" ""
              ]
              []
          ]
      , Center.markdown "600px" end
      ]
    ]

beginning = """

# Advanced Topics

It is not really necessary to know any of this stuff to be great with Elm. This
is mostly about academic work that influenced Elm or was done for Elm itself.


## Records

Read [this][records] for an overview of how records work in Elm.

Elm's record system is based on Daan Leijen's paper [Extensible records with
scoped labels][daan]. We use a somewhat restricted form of this design. The
type system works the same, but there is not syntax for field addition and
deletion. You can read about why [here][16].

[records]: /docs/records
[daan]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf
[16]: /blog/compilers-as-assistants#simplified-records


## Concurrency

Elm is moving towards a heavier emphasis on concurrency. You can read a bit
about where we are and where we are going in [the documentation][docs] of the
`Process` module.

[docs]: http://package.elm-lang.org/packages/elm-lang/core/4.0.0/Process

The following talk will give some additional perspective that guides The Elm
Architecture and the move towards explicit concurrency in general:

"""

middle = """

## Functional Reactive Programming

Elm has its roots in Evan Czaplicki's thesis work on FRP. The details are not
important as of Elm 0.17, but it may be interesting to look into the history
anyway!

The term *Functional Reactive Programming* (FRP) has been used in many ways.
This following talk is a nice overview of the major flavors of FRP.

"""

end = """

Elm uses a flavor of FRP described in Evan Czaplicki's thesis [Concurrent
FRP][conc]. It also provides a friendly overview of the academic work on FRP.

That thesis was distilled into [Asynchronous FRP][async] which was published
at PLDI 2013. It is basically a harder to read version of the thesis.

[conc]: /assets/papers/concurrent-frp.pdf
[async]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html

"""
