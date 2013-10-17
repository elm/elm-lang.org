
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

 * Switch to a [native string representation](http://docs.elm-lang.org/library/String.elm)
   that is significantly faster.
 * Allow custom precedence and associativity for infix operators.
 * Fix silly bugs and make some small improvements.

This release also includes a 

docs with mgold, maxsnew, and jsl

alex noriega List.repeat

Elm needs more than a compiler to be nice to use. This includes tools such as
[nicer docs](http://docs.elm-lang.org/) and [hot-swapping](/blog/Interactive-Programming.elm).
I have also been experimenting with &ldquo;traditional webapps&rdquo; in Elm. The first
experiment was [TodoFRP](https://github.com/evancz/todofrp) proved that the basics work.
The [new documentation](http://docs.elm-lang.org) also has an instant
search feature that was a great use for FRP. Both left me feeling like Elm can be
a great fit for traditional webapps, and I am excited to see how far we can push
in this direction.

Okay, let's talk specifics!

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

Let's see how it works! Elm&rsquo;s signal operators are defined like this:

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

Left associativity is the default because we tend to read from left to right, but
sometimes right associativity is very useful. Boolean *or* is a great example!

```haskell
a || b = if a then True else b
infixr 2 ||

falseLeft  = (True || False) || False
falseRight = True || (False || False)
```

Where you add the parentheses does not change the result, but since `(||)`
[short ciruits](http://en.wikipedia.org/wiki/Short-circuit_evaluation), it
*does* change how much computation needs to be done. Making `(||)` right
associative ensures that we use the faster way when parentheses are left off.

You can even do all this with infix functions:

```haskell
infixl 7 `div`

fortyTwo = 84 `div` 2
```

Fun fact: [all of the operators in the standard library](http://docs.elm-lang.org/InfixOps.elm)
are defined this way now. 

## New Documentation

[docs.elm-lang.org](http://docs.elm-lang.org/) is the new home of documentation
for the standard libraries. It will eventually host docs for community
libraries too, making it the place to go to discover and learn new libraries.

My favorite part this project is the search bar. It lets you live search the standard
library for modules, functions, and operators like `<~`. I have not seen something
like this in other documentation, and I think that is because it would be
significantly more complicated without FRP. The code for this feature essentially
says &ldquo;show the search results&rdquo; and it just does the right thing.

[The source code for the docs site](https://github.com/evancz/docs.elm-lang.org) is available
if you want to look into how search works or use the site as a basis for your own project.

## Fixes, Improvements, Thank yous

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
