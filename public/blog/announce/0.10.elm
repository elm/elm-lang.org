
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


## Final Notes

Huge thank you to Prezi and the community on the [elm-discuss lists][list].
The diversity of opinions and experiences on the list is extremely helpful
for Elm. I find that bringing an idea up on the lists always results in a
thoughtful discussion and ultimately leads to more refined design choices,
so thank you!

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

Now for some 0.9 specifics. Thank you to Andrew who added `as` patterns and
type annotations in let expressions. And thank you to Max New who significantly
sped up this website.

Thank you to the beginners who came to my programming class in Budapest.
Not only was it super fun, but you found a bug in the compiler!
This experience also convinced me that unary negation was a good idea.

|]
