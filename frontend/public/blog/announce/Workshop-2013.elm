import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List
import Markdown
import Signal exposing (Signal, (<~))
import Text
import Website.Skeleton exposing (skeleton)
import Window

port title : String
port title = "Elm Workshop 2013"


main : Signal Element
main =
  skeleton "Blog" everything <~ Window.dimensions


everything : Int -> Element
everything wid =
    let w = min 600 wid
    in
        flow down
          [ width w intro
          , talks w
          , width w spacetime
          ]


intro = Markdown.toElement """

<h1><div style="text-align:center">Elm Workshop
<div style="font-size:0.5em;font-weight:normal">November 9th in Budapest</div></div>
</h1>

The first ever Elm Workshop is meant to bring the Elm community together
to share what we are working on. Our goal is to
encourage discussion and push FRP and Elm forward.
Join us!

### Sessions
"""


talks w =
  flow down <|
    List.map (talk w)
    [ ("Elm&rsquo;s future", "Evan Czaplicki")
    , ("WebWorkers and Concurrent FRP", "John P. Mayer Jr.")
    , ("Preemptive Concurrency in JS", "Jaakko Pallari")
    , ("Signal Loops", "Jeff Smits")
    , ("Real-time collaboration: Elm + Firebase", "Dénes Harmath")
    , ("Visual programming with Signals", "Tim Hobbs")
    , ("Bret Victor-style reactive debugging", "Laszlo Pandy")
    , ("Making it easier to learn Elm", "Mads Flensted-Urech")
    , ("TypeScript for native Elm modules", "Laszlo Pandy")
    ]


talk w (title,speaker) =
    let title' = Text.plainText title
        speaker' = Text.leftAligned << Text.color Color.darkGrey <| Text.fromString speaker
        padding = 30
        lineHeight = heightOf title' + 4
        speakerWidth = widthOf speaker'
    in
        flow right
          [ collage padding lineHeight [ filled Color.black (circle 2) ]
          , container (w - speakerWidth - 2*padding) lineHeight midLeft title'
          , container speakerWidth lineHeight midRight speaker'
          ]


spacetime = Markdown.toElement """
If you have something to share, please propose a session on [the mailing
list](https://groups.google.com/forum/#!forum/elm-discuss).

### Where

[Prezi HQ (Merkur Palota, 3rd floor)](http://goo.gl/maps/Bul9j)<br/>
Nagymező utca 56<br/>
1065 Budapest, Hungary

### When

Saturday, November 9th at 10am.<br/>Join us at 9am for coffee.

"""
