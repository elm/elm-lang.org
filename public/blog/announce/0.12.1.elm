
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
this post, but the big takeaway is that you can have immutibility *and* speed.

 [xash]: https://github.com/xashili
 [array]: http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.1/Array

This release also simplifies all JavaScript related libraries.
With [the release of ports in 0.11](/blog/announce/0.11.elm), it became much
easier to communicate with JavaScript, so some older libraries became redundant
and confusing. As part of this overhaul, it is now possible to send arbitrary
JSON between Elm and JS, whether it has nice type or not. For folks waiting to
send arbitrary ADTs along ports, this is a plausible stop-gap measure because
recursive ADTs can be represented in JSON.

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
leafs in your tree *n* increases, this branching factor becomes really
important in practice!

To make this more precise, we can use the following formula to describe the
number of nodes you need to pass through as you vary your branching factor *b*
and number of leafs *n*.

<div style="width:100%; font-size:2em; text-align:center; font-family: 'times new roman', serif">log<sub>*b*</sub>(*n*)</div>

The trick to making immutable array lookup really fast is to make this come out
to a nice number for all reasonable workloads. The approach popularized by Scala
and Clojure is to make the branching factor 32. Let's see how many nodes we need
to traverse when there are 1 billion leafs:

<div style="width:100%; font-size:2em; text-align:center; font-family: 'times new roman', serif">log<sub>32</sub>(1 billion) ≈ 6</div>

That means that for any operation that happens in practice, it is going to take
6 steps or less! The particular approach used in the current implementation for
Elm does [additional tricks][rrbt] to make append really fast as well. To get a
more complete picture of how to optimize this further, definitely watch
[Zach&rsquo;s talk][infoq].

## Json

## Thank you!


|]
