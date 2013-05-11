
import Website.Skeleton
import Website.ColorScheme
import Window as Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.8")
foreign export jsevent "elm_title"
  title : Signal JSString

main = lift (skeleton intro) Window.width

intro w = width w [markdown|

<style type="text/css">
p { text-align: justify }
h2,h3,h4 { padding-top: 0.5em; }
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

<h1><div style="text-align:center">Elm 0.8
<div style="font-size:0.5em;font-weight:normal">*Improving everything*</div></div>
</h1>

A new release.

* Type annotations and type aliases
* More efficient function calls and ADT representation
* Improve the `collage` API
* Allow dynamic creation of GUI inputs
* Better JS integration

|]
