import Blog
import Center


main =
  Blog.blog
    "Elm 0.10.1"
    "Tools and Libraries"
    Blog.evan
    (Blog.Date 2013 12 30)
    [ Center.markdown "600px" content ]


content = """

A lot of work is going into tooling right now, so this incremental release
mainly gets the compiler in shape for:

  * [A greatly improved version of `elm-repl`](https://github.com/elm-lang/elm-repl/blob/master/changelog.txt#L1-L12)
    (`cabal install elm-repl`)
  * Easily sharing Elm libraries (announcement coming soon!)

Besides that stuff, this release focuses on improving Elm's standard libraries.
New and improved libraries include:

  * [`List`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/List) &mdash;
    add general sorting functions
  * [`Transform2D`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Transform2D) &mdash;
    greatly expanded API
  * [`Bitwise`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Bitwise) &mdash;
    for your bitwise operation needs
  * [`Regex`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Regex) &mdash;
    for when parser combinators are too much

There are also many miscellaneous fixes and improvements. Most notably,
infinite types lead to *much* nicer error messages, type errors should
be a bit easier to read, and stale intermediate files are detected automatically.
You can install 0.10.1 with [these instructions](/install)
or upgrade with:

    cabal update ; cabal install elm

The rest of this post covers the improvements directly related to the compiler
and core libraries.

## Sorting

In addition to a standard `sort` for any comparable values,
[the list library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/List)
can now do some more flexible sorting with
[`sortBy`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/List#sortBy) and
[`sortWith`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/List#sortWith).
First, `sortBy` lets you sort values by a derived property:

```elm
sortBy : (a -> comparable) -> [a] -> [a]

sortBy String.length ["mouse","cat"] == ["cat","mouse"]
```

This makes it really easy to do relatively involved sorts on
lists of records or other complex values:

```elm
alice = { name="Alice", height=1.62 }
bob   = { name="Bob"  , height=1.85 }
chuck = { name="Chuck", height=1.76 }

sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
sortBy .height [chuck,alice,bob] == [alice,chuck,bob]
```

If that's not general enough for you, `sortWith` lets you sort
values with a custom comparison function:

```elm
sortWith : (a -> a -> Order) -> [a] -> [a]

sortWith (flip compare) [1..5] == [5,4,3,2,1]
sortWith personCompare [chuck,alice,bob] == [alice,bob,chuck]

-- compare by name first, compare by height to break ties
personCompare a b =
    case compare a.name b.name of
      EQ -> compare a.height b.height
      order -> order
```

Big thank you to [Max Goldstein](https://github.com/mgold) for suggesting
and implementing this and to [Max New](https://github.com/maxsnew) for
coming up with really nice names for both functions. I am far too excited
about the `sortBy` function.

## Transform2D, Bitwise, and Regex

[`Transform2D`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Transform2D)
was significantly fleshed out by [Michael Søndergaard](https://github.com/Sheeo)
(Thank you!). Using `groupTransform` should be quite a bit more pleasant now.

I added the [`Bitwise`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Bitwise)
library for low-level bit manipulations of integers.
This may come in handy if you are writing a random number generator,
as [Joe Collard is](https://github.com/jcollard/elm-random).

I also added the [`Regex`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Regex)
library for searching through strings. It seemed like
a logical step after Elm [got a proper string representation in
0.10](/blog/announce/0.10). Note: I decided to call it `Regex` rather
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

Thank you again to everyone who contributed to this release!

"""
