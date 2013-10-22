
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.10")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  flow down [ width w intro
                  , talks w
                  , width w spacetime ]

intro = [markdown|

<style type="text/css">
p { text-align: justify }
</style>

<h1><div style="text-align:center">Elm Workshop
<div style="font-size:0.5em;font-weight:normal">*November 9th in Budapest*</div></div>
</h1>

The first ever Elm Workshop is meant to bring the Elm community together
to share what we are working on. Our goal is to
encourage discussion and push FRP and Elm forward.
Join us!

### Sessions
|]

talks w =
  flow down <| map (talk w)
    [ ("Elm&rsquo;s future", "Evan Czaplicki")
    , ("Bret Victor-style reactive debugging", "Laszlo Pandy")
    , ("Signal loops", "Jeff Smits")
    , ("Visual programming with Signals", "Tim Hobbs")
    , ("Making it easier to learn Elm", "Mads Flensted-Urech")
    , ("TypeScript for native Elm modules", "Laszlo Pandy")
    ]

talk w (title,speaker) =
    let title' = plainText title
        speaker' = text . Text.color darkGrey <| toText speaker
        padding = 30
        lineHeight = heightOf title' + 4
        speakerWidth = widthOf speaker'
    in  flow right [ collage padding lineHeight [ filled black (circle 2) ]
                   , container (w - speakerWidth - 2*padding) lineHeight midLeft title'
                   , container speakerWidth lineHeight midRight speaker'
                   ]

spacetime = [markdown|
If you have something to share, please propose a session on [the mailing
list](https://groups.google.com/forum/#!forum/elm-discuss).

### Where

[Prezi HQ (Merkur Palota, 3rd floor)](http://goo.gl/maps/Bul9j)<br/>
Nagymező utca 56<br/>
1065 Budapest, Hungary

### When

Saturday, November 9th at 10am.<br/>Join us at 9am for coffee.

|]
