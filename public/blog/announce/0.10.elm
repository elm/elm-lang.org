
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
Otherwise it is mostly smaller bug fixes and additions.

## Native Strings

Native strings are used by default in SML, OCaml, Agda, Idris, and all other
languages from the ML-family I could find, whether they came before or
after Haskell. In previous releases of Elm, strings were represented by a list of characters:

<div style="text-align:center; font-size:2em;">`String = [Char]`</div>

I really love this isomorphism. It is simple and elegant. It is one of the
many things that I got really excited about while reading
[Learn You a Haskell](http://learnyouahaskell.com/).
It is also not a great default for strings.

Character lists are relatively slow, but the real flaw is that they expose
implementation details. This means that switching to a faster representation
is a breaking change. It becomes harder and harder to make that change as a
language grows, and I think it is not too late for Elm. Elm 0.10 introduces
[a library specifically for strings](http://docs.elm-lang.org/library/String.elm).
It is significantly faster and provides many new string-specific functions.

Overall, I have been really happy with this change. In my experience, it leads
to two kinds of breaking changes, both pretty easy to fix.

  1. You will need to swap out `List` functions for their corresponding
     `String` function. This means `map` becomes `String.map`, `filter`
     becomes `String.filter`, etc.

  2. If you pattern match on character lists, you need to start using the
     following function to get equivalent behavior:<br/>
     <div style="text-align:center; font-family: monospace; padding-top: 4px;">[uncons](http://docs.elm-lang.org/library/String.elm#uncons) : String -> Maybe (Char,String)</div>

In case two, the transformation will look something like this:

```haskell
lengthA : [Char] -> Int
lengthA charList =
    case charList of
      hd::tl -> 1 + lengthA tl
      []     -> 0

lengthB : String -> Int
lengthB string =
    case String.uncons string of
      Just (hd,tl) -> 1 + lengthB tl
      Nothing      -> 0
```

Also, note that using [`String.length`](http://docs.elm-lang.org/library/String.elm#length)
will be significantly faster than either `lengthA` or `lengthB`.

Overall, the changes I had to make for [elm-lang.org](/) and
[docs.elm-lang.org](http://docs.elm-lang.org) were not terrible and
the type checker helped a lot. If you have any trouble with the switch,
please ask about it on the [mailing list](https://groups.google.com/forum/#!forum/elm-discuss).

## Final Notes


|]
