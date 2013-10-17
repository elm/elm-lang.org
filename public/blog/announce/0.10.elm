
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.10 - Native Strings")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  width w intro

intro = [markdown|

<style type="text/css">
p { text-align: justify }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: #f8f8f8; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: #f8f8f8; }
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

<h1><div style="text-align:center">Elm 0.10
<div style="font-size:0.5em;font-weight:normal">*Native strings and custom infix ops*</div></div>
</h1>

New stuff includes:

 * [Strings](#native-strings) &mdash; switch to a
   [native representation](http://docs.elm-lang.org/library/String.elm)
   that is significantly faster
 * [Infix Ops](#infix-operators) &mdash; support custom precedence and associativity
 * [Loose ends](#loose-ends) &mdash; tons of small improvements and bug fixes

There are also some improvements for Elm related tools including
[nicer docs](http://docs.elm-lang.org/) and
[hot-swapping](/blog/Interactive-Programming.elm) in the online editor.

I have also been experimenting with &ldquo;traditional webapps&rdquo; in Elm.
Two notable experiments are:

 * [TodoFRP](https://github.com/evancz/todofrp) &mdash; easy to put everything together,
   FRP worked very nicely, but missing some knobs for aesthetics.
 * [Instant search feature in docs](http://docs.elm-lang.org) &mdash; wonderful use for FRP!
   I'll talk about this more [lower down](#new-documentation).

Both left me feeling like Elm can be a great fit for traditional webapps, so
I am excited to see how far we can push in this direction.

Okay, let's talk specifics of 0.10!

## Native Strings

This release moves from the Haskell inspired list of characters to [a native
string representation](http://docs.elm-lang.org/library/String.elm) that is
significantly faster and provides many new string-specific functions.

<div style="text-align:center; font-size:2em;">`String ≠ [Char]`</div>

Character lists are relatively slow, and they expose implementation details
that make it hard to upgrade to a faster representation or even optimize for
common uses of strings. The [new String library](http://docs.elm-lang.org/library/String.elm)
is properly abstracted so the underlying representation can be
optimized or changed without changing the API.

Overall, the changes I had to make to upgrade [elm-lang.org](/) and
[docs.elm-lang.org](http://docs.elm-lang.org) were fairly minimal.
In my experience there were two kinds things that I needed to fix.

#### 1. Switch to String functions

You will need to swap out `List` functions for their corresponding
`String` function. So `map` becomes `String.map`, `filter`
becomes `String.filter`, etc. This is fairly straight-forward and the
type checker helps a ton.

#### 2. Pattern matching with uncons

Pattern matching now happens with
[`uncons`](http://docs.elm-lang.org/library/String.elm#uncons) which
destructures strings without exposing any implementation details.
For example, finding the length of a string looks like this:

```haskell
uncons : String -> Maybe (Char,String)

length string =
    case uncons string of
      Just (hd,tl) -> 1 + length tl
      Nothing      -> 0
```

I mean, [`String.length`](http://docs.elm-lang.org/library/String.elm#length)
is asymptotically faster than this, but the point is that you can still do
exactly the same stuff as before with minor syntactic changes.
I should also note that I got this `uncons` trick
from the many Haskell libraries that did it first&mdash;`Parsec`, `Text`,
`ByteString`&mdash;and I look forward to seeing it used in parser combinator
libraries in Elm.

If you have any trouble with the switch, please ask about it on the
[mailing list](https://groups.google.com/forum/#!forum/elm-discuss)!

## Infix Operators

You now can set the
[precedence](http://en.wikipedia.org/wiki/Order_of_operations) and
[associativity](http://en.wikipedia.org/wiki/Operator_associativity)
of custom infix operorators. This makes it easier to use [embedded
DSLs](http://c2.com/cgi/wiki?EmbeddedDomainSpecificLanguage). Hypothetical
examples include D3 bindings and a parsing library ;)
Let's see how it works, taking Elm&rsquo;s signal operators as an example:

```haskell
f <~ s = lift f s
sf ~ s = lift2 (<|) sf s

infixl 4 <~
infixl 4 ~
```

This declares that the `(<~)` and `(~)` operators are left associative and have
precedence four. &ldquo;Left associative&rdquo; means parentheses are added from
the left, so the following expressions are equivalent:

```haskell
signal  = f <~ a ~ b ~ c
signal' = (((f <~ a) ~ b) ~ c)
```

Left associativity is the default, but sometimes right associativity is very useful.
Boolean *or* is a great example.

```haskell
a || b = if a then True else b
infixr 2 ||

falseLeft  = (True || False) || False
falseRight = True || (False || False)
```

Where you add the parentheses *does not* change the result,
but since `(||)` [short ciruits](http://en.wikipedia.org/wiki/Short-circuit_evaluation),
it *does* change how much computation needs to be done. Making `(||)` right
associative ensures that we use the faster way when parentheses are left off.

Fun facts: [all of the operators in the standard library](http://docs.elm-lang.org/InfixOps.elm)
are defined this way now, and you can even do all this with infix functions:

```haskell
infixl 7 `div`
```


## New Documentation

This release also comes with `elm-doc` which extracts Elm documentation into
JSON to be used however people want. I am using it to populate the new
[docs.elm-lang.org](http://docs.elm-lang.org/) site, but I designed it to
be useful for something like Hoogle or IDE tools like inline-docs or auto-completion.

The format for documentation is described [here](/learn/Documentation.elm).
Huge thank you to [Max New](https://github.com/maxsnew),
[Max Goldstien](https://github.com/mgold), and [Justin Leitgeb](https://github.com/jsl)
for helping convert the standard libraries to the new format.

My favorite part this project is [the search bar on the docs site](http://docs.elm-lang.org/).
It lets you live search the standard library for modules, functions, and operators.
Hopefully this will help newcomers find operators that are tough to Google for,
like [`(<~)`](http://localhost:8080/library/Signal.elm#<~)
and   [`(~)`](http://localhost:8080/library/Signal.elm#~).

That's all great, but the *real* best part is that it was really simple to code that
feature. I got it running in an afternoon, mainly motivated by the fact that my design
for the site had an akward amount of empty space in the sidebar. The seach code
was pretty basic, just crawling over some JSON. The graphics code essentially says
&ldquo;show the search results&rdquo; and updates just flow through as the user types.

Without FRP and pure graphics I think this feature would be *significantly* more complicated
and error prone. To be clear, the alternative is manually modifying the DOM and trying to
synchronize the model state and &ldquo;view state&rdquo;. I put &ldquo;view state&rdquo;
in quotes to point out that it is often a non-essential and error-prone part of GUIs, yet
it is a standard and uncontroversial term for talking about front-end programs.
Is this Stockholm Syndrome?

In any case, [the source code for the docs site](https://github.com/evancz/docs.elm-lang.org) is available
if you want to look into search, use the site as a starting point for your own project, or whatever else.

## Loose ends

Thanks to everyone who helped with this release, whether it was
contributions or helping talk through ideas on the [email
list](https://groups.google.com/forum/#!forum/elm-discuss)! 
The following list covers the most important fixes and improvements.

* Realiasing type errors, making them shorter and easier to read.

* Fix the `remove` function in [the `Dict`
  library](http://docs.elm-lang.org/library/Dict.elm) based on [Matt Might's
  work on this topic](http://matt.might.net/articles/red-black-delete/). Thank you
  to [Max New](https://github.com/maxsnew) for taking on this arduous task!

* The `Matrix2D` library has been renamed [`Transform2D`](http://docs.elm-lang.org/library/Transform2D.elm).
  This library is actually made up of [augmented matrices](http://en.wikipedia.org/wiki/Affine_transformation#Augmented_matrix)
  that let you represent translations, and we wanted to make that clearer.

* Pattern matching on literals was announced in 0.9, but a bug snuck in right before
  release. That is fixed now!

* Add function to `Random` library to get a list of random numbers, thanks to
  [Max Goldstien](https://github.com/mgold)!
  <p style="text-align:center; font-family:monospace;">
  [floatList](http://docs.elm-lang.org/library/Random.elm#floatList) : Signal Int -> Signal [Float]</p>

* Make compiler compatable with cabal 1.18, thanks to [Justin Leitgeb](https://github.com/jsl)!

* Fix bug in functions that take 10 or more arguments, thanks to [Max New](https://github.com/maxsnew)

* Switch to `language-ecmascript` for generating JS. If you generate JS from Haskell, you
  should switch too. This is a good library

|]
