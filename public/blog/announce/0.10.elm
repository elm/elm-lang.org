
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
<div style="font-size:0.5em;font-weight:normal">*Native strings and incremental fixes*</div></div>
</h1>

By far the biggest change in this release is a switch to a [native string
representation](http://docs.elm-lang.org/library/String.elm).
This release also brings a wide range of bug fixes and minor improvements,
such as realiasing in type errors and a patch for pattern matching on literals.

Elm needs more than a compiler to be nice to use, so I have also been working
on improving tools and examples. 

  * **Tools:** [nicer docs](http://docs.elm-lang.org/) and [hot-swapping](/blog/Interactive-Programming.elm)
  * **Examples:** [TodoFRP](https://github.com/evancz/todofrp) and [docs source code](https://github.com/evancz/docs.elm-lang.org)

For examples, I have been experimenting with &ldquo;traditional webapps&rdquo; to see
how easy it is to work with lots of input boxes and buttons. The first experiment was
[TodoFRP](https://github.com/evancz/todofrp) which is instructive but needs more polish.
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

## Fixes and Improvements

* Matrix2D becomes Transform2D
* type realiasing
* fix for pattern literals

## New Documentation

[docs.elm-lang.org](http://docs.elm-lang.org/) is the new home of documentation
for the standard libraries. It will eventually host docs for community
libraries too, making it the place to go to discover and learn new libraries.

My favorite part this project is the search bar. It lets you live search the standard
library for modules, functions, and operators like `<~`. I have not seen something
like this in other documentation, and I think that is because it would be
significantly more complicated without FRP. The code for this feature essentially
says &ldquo;show the search results&rdquo; and it just does the right thing.

This site also includes [a table of operators](http://docs.elm-lang.org/InfixOps.elm) showing the
precedence and associativity of all infix ops in the standard library. This information is also
listed right in the documentation now: see [`(+)`](http://docs.elm-lang.org/library/Basics.elm#+) and
[`(<~)`](http://docs.elm-lang.org/library/Signal.elm#<~).

[The source code for the docs site](https://github.com/evancz/docs.elm-lang.org) is available
if you want to look into these features or use it as a basis for your own project.

## Final Notes


|]
