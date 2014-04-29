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

Elm now has *fast* immutable arrays. How can that be? Is there such a
thing? Thanks to [Christian Widera][xash], the new [`Array` library][array]
uses some very clever data structures that make common operations like `get`
and `set` constant time in practice! We will get into the details later in
this post, but the big takeaway is that you can have immutability *and* speed.
The new `Array` library instigated a push for consistency across all data
structures, so there are some changes and improvements in the `Dict` and `Set`
libraries too.

 [xash]: https://github.com/xashili
 [array]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Array

This release also simplifies all JavaScript related libraries.
With [the release of ports in 0.11](/blog/announce/0.11.elm), it became much
easier to communicate with JavaScript, so some older libraries became redundant
and confusing. After deleting a bunch of code, [the `Json` library][json] came
out simpler and more useful. This release also makes it possible to send
arbitrary JSON through ports, so we will cover that too.

 [json]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Array

## Arrays

This library was inspired by Zach Allaun&rsquo;s great talk&mdash;[Functional
Vectors, Maps, and Sets in Julia][infoq]&mdash;which explains the data
structures and clever optimizations behind immutable arrays very clearly.
[Christian Widera][xash] ran with these ideas, implementing [Relaxed Radix
Balanced Trees][rrbt] for Elm.

 [infoq]: http://www.infoq.com/presentations/julia-vectors-maps-sets
 [rrbt]: http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf%E2%80%8E

#### High-level Overview

Making an immutable data structure fast is often a matter of figuring out
how to represent it as some kind of tree. One way to make these trees faster
is to increase the &ldquo;branching factor&rdquo;.

<img src="/trees.png"
     style="width:500px; height:200px; display:block; margin: 0 auto;">

The tree on the left has a branching factor *b* of 2. This means you need to
go through three nodes to get to a leaf. The tree on the right has a branching
factor *b* of 8, so you only need to go through one node! As the number of
leaves in your tree *n* increases, this branching factor becomes really
important!

To make this more precise, we can use the following formula to describe the
number of nodes you need to pass through as you vary your branching factor *b*
and number of leaves *n*.

<div style="width:100%; font-size:2em; text-align:center; font-family: 'times new roman', serif">log<sub>*b*</sub>(*n*)</div>

The trick to making immutable array lookup really fast is that the constant *b*
actually makes a huge difference in practice even though it technically does not
change the asymptotic complexity of the data structure. This approach has been
popularized by Scala and Clojure which use a branching factor of 32. Let's see
how many nodes we need to traverse when there are 1 billion leaves (*n* = 1
billion, *b* = 32):

<div style="width:100%; font-size:2em; text-align:center; font-family: 'times new roman', serif">log<sub>32</sub>(1 billion) ≈ 6</div>

That means that for any operation that happens in practice, it is going to take
6 steps or less! The particular approach used in the current implementation for
Elm does [additional tricks][rrbt] to make appending arrays really fast as well.
To get a more complete picture of how to optimize this further, watch
[Zach&rsquo;s talk][infoq] and read the paper on [Relaxed Radix Balanced
Trees][rrbt].

#### Consistency across Data Structures

Both `Array` and `Dict` have lookup functions that may fail, so we have
standardized the function names across APIs. This is a breaking change for
`Dict`:

```haskell
get       : comparable -> Dict comparable v -> Maybe v
getOrElse : v -> comparable -> Dict comparable v -> v
getOrFail : comparable -> Dict comparable v -> v
```

Also, thanks to [Harry Garrood](https://github.com/hdgarrood) the `Dict` and
`Set` libraries now support `filter` and `partition`.

## Json

[The `Json` library][json] has been simplified quite dramatically. JSON is now
represented by the `Json.Value` type:

 [json]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Json

```haskell
data Value
    = String String
    | Number Float
    | Boolean Bool
    | Null
    | Array [Value]
    | Object (Dict.Dict String Value)
```

The most valuable part of this change is that you can send a `Json.Value`
through a port:

```haskell
port randomInternetData : Signal Json.Value
```

This makes it possible to handle JavaScript values that have unreliable
structure. Even if the &ldquo;type&rdquo; of an incoming value is
&ldquo;sometimes a string, other times an object with between 3 and 5
fields&rdquo; you can represent it as a `Json.Value`.

This also means that you can send deeply nested values out through ports.
The following code takes a recursive data structure that represents text
that may be **bold** and turns it into JSON:

```haskell
data PrettyText =
    Text String | Concat [PrettyText] | Bold PrettyText

toJson : PrettyText -> Json.Value
toJson prettyText =
  case prettyText of
    Text str  -> Json.String str
    Concat ts -> Json.Array (map toJson ts)
    Bold text ->
        Json.Object <| Dict.singleton "bold" (toJson text)

port prettyText : Signal Json.Value
port prettyText = toJson <~ prettyTexts
```

It would *definitely* be nicer to to just allow ADTs to travel through ports
with some systematic conversion to JavaScript objects. Well, that is [in the
pipeline][issue]! But until that is implemented, `Json.Value` at least makes
it *possible* to send recursive ADTs through ports.

 [issue]: https://github.com/elm-lang/Elm/issues/490

## Thank you!

Huge thank you to [Christian Widera][xash] who implemented the new `Array`
library. Thanks to Zach Allaun for the inspiration.

Another huge thank you to [Max New](https://github.com/maxsnew) and [Alex
Neslusan](https://github.com/deadfoxygrandpa) for drastically improving test
coverage in the compiler:

  * Max&rsquo;s [IO](https://github.com/maxsnew/IO) library makes it possible to
    work with stdin and stdout.
  * Alex&rsquo;s [Elm-Test](https://github.com/deadfoxygrandpa/Elm-Test)
    unit-testing framework makes it easy to write tests for Elm programs.

Together they make it possible to [set up continuous integration tests with your
Elm project](https://groups.google.com/forum/#!searchin/elm-discuss/travis$20unit$20test/elm-discuss/AAWsF7hDbA4/8zSrSG2-FHUJ)!

Thanks to [Max New](https://github.com/maxsnew) for speeding up a bunch of
functions in the `List` library.

Finally, thanks to [Attila Gazso](https://github.com/agazso) for creating [Mac
and Windows installers for Elm](https://github.com/elm-lang/elm-platform).
Hopefully we can keep going with this effort and make the [Elm
Platform](https://github.com/elm-lang/elm-platform) easy to install on any
system.

|]
