import Website.Skeleton (skeleton)
import String
import Window

main = skeleton "Libraries" content <~ Window.dimensions

content outer =
    let inner = 600
        half = inner `div` 2
        center elem =
            container outer (heightOf elem) middle elem
        centerText msg =
            let msg' = width inner msg
            in  center msg'
    in
      flow down
      [ centerText intro
      , spacer outer 20
      , center (flow right [ standardLibs 260, spacer 40 10, communityLibs 260 ])
      , spacer outer 20
      , centerText outro
      ]

intro = [markdown|

# Libraries

The libraries all live at [library.elm-lang.org](http://library.elm-lang.org).
When looking at a particular library, use the search feature to find
documentation for functions like `map` and `length` or operators like `/=` and
`|>`.

|]

outro = [markdown|

See the [syntax reference](/learn/Syntax.elm) and [other learning
resources](/Learn.elm) to learn more about the language itself.

|]

standardLibs : Int -> Element
standardLibs w =
    link "http://library.elm-lang.org/catalog/elm-lang-Elm/latest/" <|
    flow down
    [ container w 40 middle (leftAligned . Text.height 20 <| toText "Standard Libraries")
    , image w w "/screenshot/Home/Catalog.png"
    ]

communityLibs : Int -> Element
communityLibs w =
    link "http://library.elm-lang.org/catalog" <|
    flow down
    [ container w 40 middle (leftAligned . Text.height 20 <| toText "Community Libraries")
    , image w w "/screenshot/Home/Catalog.png"
    ]
