import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import TopBar
import Outline


main =
  div []
    [ TopBar.topBar "docs"
    , Center.markdown "600px" quickStart
    , div [ Center.style "600px" ]
        [ h1 [id "complete-guide"] [text "Complete Guide"]
        , ul [class "guide content"] (List.map viewChapter Outline.outline)
        ]
    , Center.markdown "600px" advancedStuff
    ]


quickStart = """

# Documentation

### Quick Start

  * [For JS users](/docs/from-javascript)
  * [Make an HTML app](https://github.com/evancz/start-app)
  * [The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/)

### References

  * [Syntax](/docs/syntax)
  * [Style Guide](/docs/style-guide)
  * [Core Libraries](http://package.elm-lang.org/packages/elm-lang/core/latest/)
  * [Community Packages](http://package.elm-lang.org)
  * [Package Design](http://package.elm-lang.org/help/design-guidelines) /
    [Documentation](http://package.elm-lang.org/help/documentation-format)

"""


advancedStuff = """

# Advanced Topics

  * [Extensible Records][ext] &mdash; A full overview of how records work in Elm.
    Gets into details of Daan Leijen's [paper][daan] that first described this
    design.

  * [Concurrent FRP][conc] &mdash; Evan&rsquo;s senior thesis on Elm. Includes
    a very accessible history of FRP and overview of how signals work in Elm.

  * [Asynchronous FRP][async] &mdash; The formal semantics of Elm from
    PLDI 2013. This overlaps quite a bit with Concurrent FRP for GUIs but is
    more focused and dryer in tone.


[ext]: /docs/records
[daan]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf
[conc]: /papers/concurrent-frp.pdf
[async]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html

"""


viewChapter : (String, List String) -> Html
viewChapter (title, sections) =
  li []
    [ a [href ("/guide/" ++ format title)] [text title]
    , ul [] (List.map (\name -> li [] [ text name ]) sections)
    ]


format str =
  String.toLower str
    |> String.map (\c -> if c == ' ' then '-' else c)