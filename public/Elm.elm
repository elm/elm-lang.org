import Website.Widgets (bigLogo, installButtons, button)
import Website.Skeleton (skeleton)
import Website.BigTiles as Tile
import Website.ColorScheme as C

import Text
import Window

port title : String
port title = "Elm - functional web programming"

main = skeleton "" content <~ Window.dimensions

tagLine =
    leftAligned <|
        toText "A " ++
        Text.link "/learn/What-is-FRP.elm" (toText "functional reactive") ++
        toText " language for interactive applications"

content outer =
    let inner = 600
        half = inner `div` 2
        center elem =
            container outer (heightOf elem) middle elem
        centerText msg =
            let msg' = width inner msg
            in  center msg'
    in
    color white (flow down
    [ spacer outer 20
    , container outer 100 middle bigLogo
    , container outer 40 middle tagLine
    , center (installButtons 440)
    , spacer outer 20
    ]) `above` flow down
    [ color C.mediumGrey (spacer outer 1)
    , spacer outer 30
    , center threeKeywords
    , spacer outer 36
    , centerText exampleText
    , container outer 460 middle <| exampleBlock 860
    , center (button outer 260 "/Examples.elm" "More Examples")
    , spacer outer 36
    , width outer debuggerTitle
    , centerText debuggerText
    , center debuggerBlock
    , center <| flow right [ button 220 180 "/try" "Edit"
                           , button 220 180 "http://debug.elm-lang.org/try" "Debug"
                           ]
    ]

threeKeywords =
    flow right
    [ width 260 functional
    , spacer 40 10
    , width 260 reactive
    , spacer 40 10
    , width 260 compatible
    ]

functional = [markdown|
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 2em;">Functional</div>

Elm helps you write shorter and safer code with features like first-class
functions, immutability, and type inference. We also focus on making it [easy
to learn](/Learn.elm).

|]

reactive = [markdown|
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 2em;">Reactive</div>

Elm is based on the idea of [Functional Reactive
Programming](/learn/What-is-FRP.elm). Create highly interactive applications
without messy callbacks or a tangle of shared state.
|]

compatible = [markdown|
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 2em;">Compatible</div>

Elm compiles to HTML, CSS, and JavaScript. [Embedding in
HTML](/learn/Components.elm) and [JS interop](/learn/Ports.elm) are easy, so it
is simple to write part of your application in Elm.

|]

exampleText = [markdown|

<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 3em;">Examples</div>

Elm is great for [2D](/blog/Pong.elm) and
[3D](https://github.com/johnpmayer/elm-webgl) games,
[diagrams](https://github.com/seliopou/elm-d3), widgets, and
[websites](http://github.com/elm-lang/elm-lang.org). In addition to the larger
examples showcased here, there are tons of [educational examples](/Examples.elm)
to help you learn Elm by reading and modifying simple programs.

|]

exampleBlock w =
    Tile.examples w
    [ [ ("Home/Mario", "/edit/examples/Intermediate/Mario.elm")
      , ("Home/Elmtris", "http://people.cs.umass.edu/~jcollard/elmtris/")
      , ("Home/Vessel", "https://slawrence.github.io/vessel")
      , ("Home/FirstPerson", "https://evancz.github.io/first-person-elm")
      ]
    , [ ("Home/Catalog", "http://library.elm-lang.org/catalog/elm-lang-Elm/latest")
      , ("Home/Todo", "https://evancz.github.io/TodoFRP")
      , ("Home/Fractal", "http://gideon.smdng.nl/2014/04/fractals-for-fun-and-profit/")
      , ("Home/PieChart", "/edit/examples/Intermediate/PieChart.elm")
      ]
    ]

debuggerTitle = [markdown|

<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 3em;">Editor and Debugger</div>

|]

debuggerText = [markdown|

The combination of [functional reactive programming](/learn/What-is-FRP.elm) and
managed effects makes Elm's [Time Traveling Debugger][debug] simple and reliable.
The editor also allows [hot-swapping](/blog/Interactive-Programming.elm), so you
can modify running programs.

[debug]: http://debug.elm-lang.org

|]

debuggerBlock =
    Tile.example (596,308) ("Home/Debugger", "http://debug.elm-lang.org/")