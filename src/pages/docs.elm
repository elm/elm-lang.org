import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton


port title : String
port title =
  "Elm Docs"


main =
  Skeleton.skeleton
    "docs"
    [ Center.markdown "600px" quickStart
    , div [ Center.style "600px" ]
        [ h1 [id "complete-guide"] [text "Complete Guide"]
        , ul [class "guide content"] outline
        ]
    , Center.markdown "600px" advancedStuff
    ]


quickStart = """

# Documentation

### Quick Start

  * [For JS users](/docs/from-javascript)
  * [Make an HTML app](https://github.com/evancz/start-app)
  * [The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/)
  * [TodoMVC](https://github.com/evancz/elm-todomvc)

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

  * [Extensible Records][ext] &mdash; A full overview of how records work in Elm,
    based on the design from a [paper][daan] by Daan Leijen.

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


outline : List Html
outline =
  [ viewChapter <| local
      "Core Language"
      [ "Values"
      , "Functions"
      , "If Expressions"
      , "Lists"
      , "Tuples"
      , "Records"
      ]
  , viewChapter <| local
      "Model The Problem"
      [ "Contracts"
      , "Enumerations"
      , "State Machines"
      , "Tagged Unions"
      , "Banishing NULL"
      , "Recursive Data Structures"
      ]
  , viewChapter
      ( Link "Architecture" archUrl
      , [ archLink "Components" "example-1-a-counter"
        , archLink "Components with HTTP" "example-5-random-gif-viewer"
        , archLink "Components with Animation" "example-8-animation"
        ]
      )
  , viewChapter <| local
      "Reactivity"
      [ "Signals"
      , "Tasks"
      ]
  , viewChapter <| local
      "Interop"
      [ "HTML Embedding"
      , "Ports"
      ]
  ]


archUrl : String
archUrl =
  "https://github.com/evancz/elm-architecture-tutorial/"


archLink : String -> String -> Link
archLink name tag =
  Link name (archUrl ++ "#" ++ tag)


local : String -> List String -> (Link, List Link)
local title sections =
  let
    root = "/guide/" ++ format title
  in
    ( Link title root
    , List.map (\name -> root ++ "#" ++ format name) sections
        |> List.map2 Link sections
    )


type alias Link = { name : String, url : String }


viewChapter : (Link, List Link) -> Html
viewChapter (title, sections) =
  let
    viweSection section =
      li [] [ a [href section.url] [text section.name] ]
  in
    li []
      [ a [href title.url] [text title.name]
      , ul [] (List.map viweSection sections)
      ]


format str =
  String.toLower str
    |> String.split " - "
    |> String.join " "
    |> String.map (\c -> if c == ' ' then '-' else c)
