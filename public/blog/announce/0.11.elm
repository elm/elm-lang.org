
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.11")
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

<h1><div style="text-align:center">Elm 0.11
<div style="font-size:0.5em;font-weight:normal">*Markdown interpolation and libraries*</div></div>
</h1>

This release is a shift for Elm in the sense that a lot of the cool stuff
happening in libraries. I think Elm as a language is getting to a level
of maturity that my focus&mdash;and the focus of the community&mdash;has
shifted more towards tools and libraries. Just last week the `elm-repl`

## Markdown Interpolation

https://gist.github.com/evancz/7156716

## Library Improvements

#### Library Laziness

#### Sorting Lists

The list library just got two new functions&mdash;`sortBy` and
`sortWith`&mdash;suggested and implemented by Max Goldstein.
First, `sortBy` lets you sort values by a derived property:

```haskell
sortBy : (a -> comparable) -> [a] -> [a]

sortBy String.length ["five","hi"] == ["hi","five"]

alice = { name="Alice", height=1.62 }
bob   = { name="Bob"  , height=1.85 }
chuck = { name="Chuck", height=1.76 }

sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
sortBy .height [chuck,alice,bob] == [alice,chuck,bob]
```

This makes it really easy to do relatively involved sorts on
lists of records or other complex values. If that's not general
enough for you, `sortWith` lets you sort values with a custom
comparison function:

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

#### Transform2D, Bitwise, and Regex

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

Error messages for infinite types are nice now. The obvious thing to do is add
an occurs check during unification, but thanks to [Pottier and Remy][hmx]
I knew that this does a ton of unnecessary computation. In most cases, an infinite
type cannot possibly occur. That made sense, but it was not at all clear to me
where the occurs check goes *instead*. Unfortunately, this is the only detail that
really matters.

Eventually I got stuck on a long flight with only [that PDF][hmx] and emacs. Turns
out being locked in a room with the right tools, and crucially, *no internet* is
pretty effective. You only need to do an occurs check on the types of
named values. A value must be named to be able to appear within itself! So it works
now and it does not impose an unnecessary performance hit on type inference.

  [hmx]: http://www.cs.cmu.edu/~rwh/courses/refinements/papers/PottierRemy04/hmx.pdf

|]
