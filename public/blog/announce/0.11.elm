
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

port title : String
port title = "Elm 0.11 - Ports"

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

<h1><div style="text-align:center">Elm 0.11 &ndash; greatly improved FFI
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Ports and the &ldquo;Component Model&rdquo;*</div></div>
</h1>

The key idea for this release comes from a “component model” for using Elm in
production systems. A “component model” means you write small UI widgets or
processing units in Elm and [embed them in a traditional
project](/learn/Embde-in-HTML.elm) written in JS. So you can try
Elm out on a specific problem and see if it works for you.

<img src="/imgs/embed.png" alt="Component Model"
     style="width:480px; height:320px; margin-left: auto; margin-right: auto; display:block;">

The success stories I have heard for JVM languages like Scala and Clojure
go something like this: “There was some module that was a terrible
quagmire of state and bugs and slowness. [There did not appear to be any
solution](http://youtu.be/WzEhoyXpqzQ?t=31s). An individual rewrote it in
language X. It was a third as long, ran faster, and was easier to maintain.
There was much rejoicing and language X became the primary language of that team.”
The key components are:

 * There was a known problem that seemed intractable.
 * The problem was bad but small enough for one person to fix.
 * Approaching the problem in a functional style drastically improved
   things in a way that was clear to people who make decisions.

It is not about being theoretically better, it is about being demonstrably
better. Getting a foothold requires running small experiments and finding
problems that are particularly well-suited to purity, immutability,
reactivity, type-safety, etc. I think there is still more to improve in
Elm, but the “component model” at least makes this kind of story *possible*.

## Ports

Before this release the FFI was pretty intense and unweildy. It required lots of
keywords, felt clunky, and was limited to signals. This release introduces
[ports](/learn/Ports.elm), a cleaner and more general FFI.



|]
