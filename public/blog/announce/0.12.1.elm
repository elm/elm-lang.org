
import Website.Skeleton (skeleton)
import Website.Tiles (examples)
import Window

port title : String
port title = "Elm 0.12.1 - Arrays"

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  flow down
        [ width w intro
        ]

exampleBlock w =
    examples w [ [ "TextReverse", "Calculator", "Form", "Plot" ] ]

intro = [markdown|

<style type="text/css">
p { text-align: justify }
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

<h1><div style="text-align:center">Elm 0.12.1 - Arrays
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Fast, Immutable Data Structures*</div></div>
</h1>

Recently [Christian Widera][xash] needed an Array for a game, but found that
faking it a `List` or `Dict` was too slow.
Inspired by Zach Allaun&rsquo;s great talk on [Functional Vectors, Maps, and
Sets in Julia](http://www.infoq.com/presentations/julia-vectors-maps-sets),
Christian decided to implement fast, immutable arrays for Elm. Thanks to some
very clever data structures, common operations like `get` and `set` are constant
time in practice! We will get into the details later in this post, but the big
takeaway is that you can have immutibility *and* speed.

 [xash]: https://github.com/xashili

This release also simplifies all JavaScript related libraries.
With [the release of ports in 0.11](/blog/announce/0.11.elm), it became much
easier to communicate with JavaScript, so some older libraries became
redundant. The biggest improvement from this overhaul is that *any* valid JSON
can be sent between Elm and JS, whether it can be given a nice type or not.
For folks waiting to send arbitrary ADTs along ports, sending arbitrary JSON
is a plausible stop-gap measure because it permits recursive structures.

## Arrays

[Relaxed Radix Trees](http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf%E2%80%8E)

## Json

## Thank you!


|]
