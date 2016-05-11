import Blog
import Center


main =
  Blog.blog
    "Elm 0.10"
    "faster strings, colors, bug fixes, and searchable docs"
    Blog.evan
    (Blog.Date 2013 10 24)
    [ Center.markdown "600px" content ]


content = """

[The 0.9 release](/blog/announce/0.9) touched almost every part of
the compiler, and since then, a lot of rough patches have been discovered
and fixed. These improvements warrant a proper release on their own, but
there are also a number of important new features and improvements that
are ready for release:

 * [Strings](#strings) &mdash; switch to a
   [new representation](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/String)
   that is significantly faster
 * [Nice Colors](#nice-default-colors) &mdash; use [Tango color palette](http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines) by default
 * [Infix Ops](#infix-operators) &mdash; support custom precedence and associativity
 * [Improvements and Fixes](#improvements-and-fixes) &mdash; lots of them

There are also some improvements for Elm-related tools including
[improved documentation](http://package.elm-lang.org/),
[hot-swapping](/blog/interactive-programming) and better hints in
the online editor, and a big site redesign to make resources like
[the beginner classes](/Learn.elm),
[demo of html/js integration](https://github.com/evancz/elm-html-and-js#htmljs-integration--live-demo), and
[larger examples](/examples#open-source-projects)
easier to find.

To upgrade run `cabal update && cabal install elm`. Note that
.elmi files are *not* backwards compatible, so you must
delete `cache/` directories in existing projects.

## Strings

This release moves away from the Haskell-inspired list of characters, providing
[a new string library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/String) that
is significantly faster and provides many new string-specific functions.

<div style="text-align:center; font-size:2em;">`String ≠ [Char]`</div>

Character lists are relatively slow, and they expose implementation details
that make it hard to upgrade to a faster representation or even optimize for
common uses of strings. The [new String library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/String)
is properly abstracted so the underlying representation can be
optimized or changed without changing the API.

The changes needed to upgrade [elm-lang.org](/) and
[package.elm-lang.org](http://package.elm-lang.org/) were fairly minimal.
There were two kinds things that I needed to fix:

#### 1. Switch to String functions

You will need to swap out `List` functions for their corresponding
`String` function. So `map` becomes `String.map`, `filter`
becomes `String.filter`, etc.

#### 2. Pattern matching with uncons

Pattern matching now happens with
[`uncons`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/String#uncons) which
destructures strings without exposing any implementation details.
For example, finding the length of a string looks like this:

```elm
uncons : String -> Maybe (Char,String)

length string =
    case uncons string of
      Just (hd,tl) -> 1 + length tl
      Nothing      -> 0
```

I mean, [`String.length`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/String#length)
is asymptotically faster, but the point is that you can still do
exactly the same stuff as before with minor syntactic changes.
I should also note that I got this `uncons` trick from the many Haskell
libraries that did it first&mdash;`Parsec`, `Text`, `ByteString`&mdash;and
I look forward to seeing it used in parser combinator libraries in Elm.

## Nice Default Colors

Typically the default colors are the primary and secondary colors. These colors
are extremely aggressive and generally do not look very good alone or together.
Making pretty things should be *easy* in Elm, so
[the new color library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Color) uses
[the Tango palette](http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines)
for default colors:

<div style="width:100%; display:inline-block; text-align:center;">
<a href="http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines">
<img src="/Tango-Palette.png" style="width:456px; height:144px;" alt="Tango Palette"></img></a></div>

This color palette is designed such that all the colors work nicely
with each other. This means you can randomly slap some colors on your
project and have it look pretty good.

[The new default colors](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Color#built-in-colors)
are named red, orange, yellow, green, blue, purple, brown, grey, and charcoal. Each color
has a light and dark version. We did not use
[the official Tango names](http://tango.freedesktop.org/static/cvs/tango-art-tools/palettes/Tango-Palette.svg)
because they are harder to remember. Thank you
to [Laszlo Pandy](https://github.com/laszlopandy) for suggesting the Tango scheme!

## Infix Operators

You now can set the
[precedence](http://en.wikipedia.org/wiki/Order_of_operations) and
[associativity](http://en.wikipedia.org/wiki/Operator_associativity)
of custom infix operators. This makes it easier to use [embedded
DSLs](http://c2.com/cgi/wiki?EmbeddedDomainSpecificLanguage). Hypothetical
examples include D3 bindings and a parsing library.

The keywords `infixl`, `infix`, and `infixr` declare associativity as
left-, non-, or right- respectively. From there you add precedence and
the name of the operator. Let's see how it works, taking Elm&rsquo;s signal
operators as an example:

```elm
infixl 4 <~
infixl 4 ~
```

This declares that the `(<~)` and `(~)` operators are left associative and have
precedence four. &ldquo;Left associative&rdquo; means parentheses are added from
the left, so the following expressions are equivalent:

```elm
signal  = f <~ a ~ b ~ c
signal' = (((f <~ a) ~ b) ~ c)
```

Left associativity is the default, but sometimes right associativity is very useful.
Boolean *or* is a great example.

```elm
infixr 2 ||

falseLeft  = (True || False) || False
falseRight = True || (False || False)
```

Where you add the parentheses *does not* change the result,
but since `(||)` [short ciruits](http://en.wikipedia.org/wiki/Short-circuit_evaluation),
it *does* change how much computation needs to be done. Making `(||)` right
associative ensures that we use the faster way when parentheses are left off.

This also works for functions:

```elm
infixl 7 `div`
```

**<span style="color:rgb(234, 21, 122)">Important note!</span> Do not abuse this power!**
Use this feature *very* judiciously.
Haskell tends to use infix operators very aggressively, often in ways that hamper
readability. In Elm, you should *never* design an API with specific infix operators
in mind. Always design your API to have clear and helpful names **for everything**,
even if you know it is totally an Applicative Functor or whatever else.

Only after you are done with a fully non-symbolic API, then maybe consider the
possibility of perhaps introducing infix operators. And even if it makes
things significantly nicer, consider not adding them. Maybe wait a few releases
and see if it is necessary. Ask people to read code that uses them. Do they like
it? Can they figure it out without you? Does the symbol clarify its meaning? Can
they figure it out without seeing type signatures? I followed all of these rules
with `(<~)` and `(~)` and I am still not sure that they were a good idea.

## New Documentation

I had two major goals when working on documentation: (1) to make documentation
nice in Elm code and [online](http://package.elm-lang.org/) and (2) to make docs
useful for entirely separate projects. I want to support things like
[Hoogle](http://www.haskell.org/hoogle/)-style type search or creating
IDE features like inline-docs or autocompletion. To reach these two goals,
this release introduces:

* a new format for documentation, [described here](http://package.elm-lang.org/help/documentation-format)
* [a new home for documentation](http://package.elm-lang.org/)
* `elm-doc` which extracts Elm documentation into JSON

Now my favorite part this project is [the search bar on the docs site](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/).
It lets you live search the standard library for modules, functions, and operators.
Hopefully this will help newcomers find operators that are tough to Google for,
like [`(<~)`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Signal#<~)
and [`(~)`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Signal#~). The best part of
this feature was how simple it was to implement with FRP and Elm.

[The source code for the docs site](https://github.com/elm-lang/package.elm-lang.org)
is available if you want to look into instant search, use the site as a starting
point for your own project, or whatever else.
Also, huge thanks to [Max New](https://github.com/maxsnew),
[Max Goldstein](https://github.com/mgold), and [Justin Leitgeb](https://github.com/jsl)
for helping convert the standard libraries to the new docs format!

## Improvements and Fixes

In addition to the more obvious improvements we have seen so far, there are
tons of important fixes and improvements. Altogether, they are probably the
most significant part of this release:

* Realiasing type errors, making them shorter and easier to read. This means
  the types of `Element` and `Form` will be reported as their names instead
  of as a huge record. This is waaaay nicer.

* The `Matrix2D` library has been renamed [`Transform2D`](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Transform2D).
  This library is actually made up of [augmented matrices](http://en.wikipedia.org/wiki/Affine_transformation#Augmented_matrix)
  that let you represent translations, and we wanted to make that clearer.

* Add <span style="font-family:monospace;">
  ([Random.floatList](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Random#floatList) : Signal Int -> Signal [Float])</span><br/>
  Thanks to [Max Goldstein](https://github.com/mgold)!

* Fix the `remove` function in [the `Dict`
  library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Dict) based on [Matt Might's
  work on this topic](http://matt.might.net/articles/red-black-delete/). Thank you
  to [Max New](https://github.com/maxsnew) for taking on this arduous task!

* Switch to [`language-ecmascript`](http://hackage.haskell.org/package/language-ecmascript)
  for generating JS. This is a very nice library, and I would love for all Haskell to JS
  projects to share this backend so we can all benefit from work on optimizations or source-maps.

* Make compiler compatible with cabal 1.18, thanks to [Justin Leitgeb](https://github.com/jsl)!

* Fix bug in functions that take 10+ arguments, thanks to [Max New](https://github.com/maxsnew)

* Many more smaller improvements and fixes...

Thanks again to everyone who helped with this release, whether it was
contributions, talking through ideas on the
[list](https://groups.google.com/forum/#!forum/elm-discuss), or finding
bugs by using the compiler in new and extreme ways!

And remember, `.elmi` files *are not* backwards compatible. Delete
existing `cache/` directories with `rm -rf cache/`.

"""
