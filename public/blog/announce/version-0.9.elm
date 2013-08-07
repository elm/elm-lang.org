
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.9 - Fix the type-checker")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.width

everything wid =
    let w = min 600 wid
    in  flow down [ width w intro,
                    container w 50 middle dots,
                    spacer w 10, width w midtro,
                    container w 50 middle crosses,
                    spacer w 10, width w postCross ]

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

<h1><div style="text-align:center">Elm 0.9 &ndash; Fix the Type-Checker
<div style="font-size:0.5em;font-weight:normal">*Fast and reliable errors*</div></div>
</h1>

Build Improvements:
  * Major speed improvements to type-checker
  * Type-checker should catch _all_ type errors now
  * Module-level compilation, only re-compile if necessary
  * Import types and type aliases between modules
  * Intermediate files are generated to avoid unneeded recompilation
    and shorten compile time. These files go in ElmFiles/ by default
  * Generated files are placed in ElmFiles/ by default, replicating
    the directory structure of your source code.

Error Messages:
  * Cross-module type errors
  * Errors for undefined values
  * Pretty printing of expressions and types
 
Syntax:
  * Pattern matching on literals
  * Pattern aliases with `as` (Andrew)
  * Unary negation
  * Triple-quoted multi-line strings
  * Type annotations in let expressions (Andrew)
  * Record Constructors
  * Record type aliases can be closed on the zeroth column
  * (,,) syntax in types
  * Allow infix op definitions without args: (*) = add
  * Unparenthesized if, let, case, lambda at end of binary expressions

Libraries:
  * Detect hovering over any Element
  * Set alpha of arbitrary forms in collages
  * Switch Text.height to use px instead of em

Bug Fixes:
  * Many bug fixes for collage, especially when rendering Elements.

Website:
  * Hot-swapping
  * Much faster page load with pre-compiled Elm files (Max New)

|]
