
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Interactive Programming in Elm")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.width

everything w =
    width w intro

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

<h1><div style="text-align:center">Interactive Programming
<div style="font-size:0.5em;font-weight:normal">*Modifying Running Programs / Immediate Feedback*</div></div>
</h1>

JavaScript proved that the development loop can be as short as refreshing
your browser. By now it is obvious that this is great for quickly iterating
and testing an idea, making programming more accessible and more fun.

This was the first step towards [interactive programming][ip]: the ability to
modify and interact with programs *while they are running*.
Articles like [Learnable Programming](http://worrydream.com/LearnableProgramming/)
explore the potential of interactive programming.
Projects like [LightTable](http://www.lighttable.com/) work on the tooling,
creating an IDE for interactive programming.

 [ip]: http://en.wikipedia.org/wiki/Interactive_programming

Elm takes the next step, exploring what it means to be a *language* for
interactive programming. Elm&rsquo;s online editor now allows you to modify
running code, so you do not have to restart your program to change its behavior.
Let&rsquo;s see it in action:

<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/cI__rjCiH_k"
        frameborder="0"
        allowfullscreen
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        width="640" height="480"></iframe>
</div>

In Elm, this is called [hot-swapping](http://en.wikipedia.org/wiki/Hot_swapping).
Hot-swapping means *modifying running code*.

Support for hot-swapping is live, so you can [mess with Mario
yourself](/edit/examples/Intermediate/Mario.elm)
and play with the bouncing ball below. As you tweak the colors, shapes,
and physics in the code, you will see the ball update automatically.

<iframe src="/edit/examples/Reactive/Bounce.elm?cols=100%25%2C150px"
        frameborder="0"
        style="overflow:hidden; height:400px; width:100%"
        height="400px" width="100%"></iframe>

There are [many more examples](/examples/Intermediate.elm), so you can
continue to explore Elm and hot-swapping.

Perhaps the most interesting thing about adding hot-swapping to Elm was that it
was quite easy. It took about four days.

During those four days, it became very clear that the practicality of supporting
interactive programming was directly related to the abstractions (or lack-thereof) in
the language.

## How Hot-Swapping Works in Elm

signal graph, nodes are associated with a pure function and an immutable value

## Language-Level advantages of Elm

Not all languages are going to work well for interactive programming. There
are a number of key language features that make hot-swapping a fundamentally better
experience in Elm.

### Pure Functions

### Immutable Values

### Static Types

In some cases it is impossible to do a hot-swap with a running program.
If the data-structures used in the program change, the new functions and old
state become incompatable.

Say we switch from representing points as tuples like `(3,4)` and move
to records like `{x=3,y=4}`. Swapping in the new functions will cause errors.

In a dynamically typed language like JavaScript, you would just swap the code and
*hope* the runtime error happens quickly and is easy to reproduce. The programmer
will be left wondering if their code was wrong or if there was a problem hot-swapping.
Perhaps the programmer starts chasing a bug that does not even exist.

In a staticly typed language like Elm, it is possible to find these problems
immediately and restart the program, which would be necessary with or without
static types. It is just that in this case, the programmer can be sure that
any error they see is an error that matters, not an accident of hot-swapping.

### Static Signal Graphs

In Elm, the structure of signal graphs is known as soon as the program starts
and does not change. Elm&rsquo;s static signal graphs are possible because
Elm does not permit signals-of-signals.

Many other FRP frameworks, particularly in imperative languages, allow signal
graphs to change over time. When the structure of the signal graph no longer
matches the the initial structure, hot-swapping becomes very difficult. It is
no longer clear how the new functions should mix with the old state.

## Interactive Programming

Elm and Erlang both support hot-swapping. Both languages only allow
pure functions and only provide immutable data. In fact, both are based
on the CoCC. This is no coincidence.

In JavaScript, side-effects can happen anywhere and program state is
scattered haphazardly throughout the codebase. To ask &ldquo;how does the initial state
of my program connect to the current state?&rdquo; barely makes sense. because you must
execute your program 

[It is not a new concept](http://www.erlang.org/), it is just hard to do in languages that
freely mix functions, data, and side-effects. In a purely functional language like Elm,
hot-swapping works quite easily and naturally.


|]
