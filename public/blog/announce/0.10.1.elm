
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.10.1")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  width w intro

intro = [markdown|

<style type="text/css">
p { text-align: justify }
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

<h1><div style="text-align:center">Elm 0.10.1
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Library Laziness and Library Improvements*</div></div>
</h1>

This incremental release focuses on mostly on libraries. The improvements and
additions include:

  * [`Lazy`](http://docs.elm-lang.org/library/Lazy.elm) &mdash;
    core tools for creating efficient lazy data structures
  * [`Lazy.Stream`](http://docs.elm-lang.org/library/Lazy/Stream.elm) &mdash;
    infinite streams to model &ldquo;pure&rdquo; signals
  * [`List`](http://docs.elm-lang.org/library/List.elm) &mdash;
    add general sorting functions
  * [`Transform2D`](http://docs.elm-lang.org/library/Transform2D.elm) &mdash;
    greatly expanded API
  * [`Bitwise`](http://docs.elm-lang.org/library/Bitwise.elm) &mdash;
    for your bitwise operation needs
  * [`Regex`](http://docs.elm-lang.org/library/Regex.elm) &mdash;
    for when parser combinators are too much

There are also many miscellaneous fixes and improvements. Most notably,
infinite types lead to *much* nicer error messages, type errors should
be a bit easier to read, and stale intermediate files are detected
automatically.

Overall, this release is intended to get Elm nice and stable for 0.11.
The next big release should include a way to easily share libraries, so
keep an eye out for the Elm Public Library and start getting *your* libraries
ready for release!

## Library Laziness

It is now possible to create efficient lazy data structures thanks to Max
New&rsquo;s [`Lazy` library](http://docs.elm-lang.org/library/Lazy.elm).
The key functions in `Lazy` are `lazy` and `force`. The `lazy` function
lets you delay evaluation of an arbitrary value by putting it in a
[thunk](http://en.wikipedia.org/wiki/Thunk_(functional_programming)).

```haskell
lazy : (() -> a) -> Lazy a

lazySum : Lazy Int
lazySum = lazy (\() -> sum [1..999999])
```

This is a way of saying we want to sum a whole boatload of numbers *eventually*.
Not now, but maybe we'll want it later. If that ever happens, we use `force`:

```haskell
force : Lazy a -> a

sum1 : Int
sum1 = force lazySum

sum2 : Int
sum2 = force lazySum
```

The first time we `force` our `lazySum` we are going to finally add
those million integers together. It may take a bit of time to get through
them all, but the cool thing about `force` is that it saves its work.
When we call `force` on `lazySum` the second time, the work
is already done and it is super quick to get the result.
More generally, this means something with type `Lazy a` will be
evaluated exactly one time.

These primitives&mdash;along with helpers like `map`, `apply`, and
`bind`&mdash;provide the fundamental mechanisms to start creating
efficient lazy data structures.

### Infinite Streams

The first example of a lazy data structure in Elm is the
[`Lazy.Stream`](http://docs.elm-lang.org/library/Lazy/Stream.elm) library,
which allows you to work with infinite streams of values. These can be used
to model &ldquo;pure&rdquo; signals, but we'll get to that soon enough!
A basic usage would be creating an infinite stream of ones:

```haskell
-- building Streams
cons : a -> (() -> Stream a) -> Stream a

ones : Stream Int
ones = cons 1 (\() -> ones)
```

That's not too crazy, but you can do some pretty mind-bending things when you
have infinite streams, like efficiently defining the complete fibonacci sequence:

```haskell
fibs = cons 0 <| \() ->
       cons 1 <| \() -> zipWith (+) fibs (tail fibs)
```

That's neat, but the real goal of streams is to model &ldquo;pure&rdquo; signals,
signals that are not influenced by the outside world. For example, you could make
an infinite stream of randomly generated numbers using
Joe Collard's [`elm-random` library](https://github.com/jcollard/elm-random).
From there you can use it however you want, even turning it into a signal with
`Stream.sampleOn`:

```haskell
randoms : Stream Float

Stream.sampleOn : Signal a -> Stream b -> Signal b

clickRandoms : Signal Float
clickRandoms = Stream.sampleOn Mouse.clicks randoms
```

One important property of these streams is that, although the stream itself
is lazy, the values *in* the stream are not. A stream of lazy values would
have type `Stream (Lazy a)`. This makes it pretty easy to see exactly how
lazy something is so you can use laziness only when you actually want it.

Max New&mdash;who created both `Lazy` and `Lazy.Stream`&mdash;is working on
a more complete post on lazy streams, so keep an eye out!

## Sorting

The list library just got two new functions: `sortBy` and
`sortWith`. First, `sortBy` lets you sort values by a derived property:

```haskell
sortBy : (a -> comparable) -> [a] -> [a]

sortBy String.length ["mouse","cat"] == ["cat","mouse"]
```

This makes it really easy to do relatively involved sorts on
lists of records or other complex values:

```haskell
alice = { name="Alice", height=1.62 }
bob   = { name="Bob"  , height=1.85 }
chuck = { name="Chuck", height=1.76 }

sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
sortBy .height [chuck,alice,bob] == [alice,chuck,bob]
```

If that's not general enough for you, `sortWith` lets you sort
values with a custom comparison function:

```haskell
sortWith : (a -> a -> Order) -> [a] -> [a]

sortWith flippedComparison [1..5] == [5,4,3,2,1]

flippedComparison a b =
     case compare a b of
       LT -> GT
       EQ -> EQ
       GT -> LT
```

Big thank you to Max Goldstein for suggesting and implementing this and to
Max New for coming up with really nice names for both functions. I
am far too excited about the `sortBy` function.

## Transform2D, Bitwise, and Regex

`Transform2D` was significantly fleshed out by Michael Søndergaard (Thank you!).
Using `groupTransform` should be quite a bit more pleasant now.

I added the `Bitwise` library for low-level bit manipulations of integers.
This may come in handy if you are writing a random number generator, as [Joe
Collard is](https://github.com/jcollard/elm-random).

I also added the `Regex` library for searching through strings. It seemed like
a logical step after Elm [got a proper string representation in
0.10](/blog/announce/0.10.elm). Note: I decided to call it `Regex` rather
than `RegExp` to distinguish between the [monstrosity that is
regex](http://stackoverflow.com/a/1732454) and [beauty and joy of regular
expressions](http://www.amazon.com/Introduction-Theory-Computation-Michael-Sipser/dp/0534950973).

## Fixes

* Nice errors on infinite types in programs and type aliases.
* Slightly friendlier type error messages.
* Errors for duplicate constructors in Algebraic Data Types (Thanks to Ben Darwin)
* Fix silly String API errors (Thanks to [Tim Hobbs](https://github.com/timthelion))
* The `--print-types` flag works every time (Thanks to [Justin Leitgeb](https://github.com/jsl))
* Warnings for corrupted and incompatable intermediate files (Thanks again to Justin
  Leitgeb who made persuasive arguments and collected data when I was being
  frustratingly conservative.)

Thank you again to everyone who contributed to this release! Thank you to anyone who
has put up with my handwringing over function names, I cannot help it.
I really want to get them all right this time around! And thank you to
everyone who attended or spoke at the [Elm Workshop](/blog/announce/Workshop-2013.elm)
in Budapest! I had a lot of fun and the talks and projects were great. I came away
with some very good ideas about how to improve the Elm runtime, and more immediately,
seeing everyones work was really inspiring and really helped me pick up the pace on
creating a tool for sharing libraries!


|]
