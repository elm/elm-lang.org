
import Website.Blog (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

pageTitle = constant (JS.fromString "Interactive Programming in Elm")
foreign export jsevent "title"
  pageTitle : Signal JS.JSString

main = lift (skeleton everything) Window.width

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w title
  , section intro
  , width w video1
  , section segue
  , width w editor
  , section rest1
  , width w video2
  , section rest2
  ]

title = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Interactive Programming</div>
<div style="font-size: 1.5em;">A New Kind of REPL</div>
</div>
|]

intro = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>

<br/>Programming is becoming more interactive.
JavaScript proved that the development loop can be as short as refreshing
your browser. More recently, [Go](http://golang.org/) made fast compilation
possible at Google scale.
Articles like [Learnable Programming](http://worrydream.com/LearnableProgramming/) are
exploring the possibilities of interactive programming while projects like
[LightTable](http://www.lighttable.com/) develop the tooling for it.
Our tools are finally allowing the kind of interactivity that makes
programming more fun and more productive.

So far this movement towards an interactive programming style lacks
a common vocabulary. I&rsquo;d like to introduce the terms *Interactive Programming*
and *Hot-Swapping* to help us better understand where we are going and the challenges
we face:

* **[Interactive Programming](http://en.wikipedia.org/wiki/Interactive_programming)
  &ndash; coding with immediate feedback**<br/>
  It makes programming easier by integrating compilation, documentation, testing,
  errors, and more into the process of coding, further tightening the development loop.
  This encompasses the general goals of Learnable Programming and LightTable.

* **[Hot-swapping](http://en.wikipedia.org/wiki/Hot_swapping) &ndash;
  modifying running code**<br/>This is a specific technique used by compilers
  and runtime systems to swap new functions and values into a program while it
  is running. This technique is required for fully Interactive Programming and is
  a key requirement for both Learnable Programming and LightTable.

Interactive programming is the goal and hot-swapping is the primary technical
challenge. I make this distinction because *hot-swapping is not always possible*
and it cannot be done *reliably* in most of today&rsquo;s programming languages.

We will be exploring the limits of hot-swapping, and how language design is
the key to making it easy and reliable. Before digging into details, let&rsquo;s
see how hot-swapping works in Elm:

|]

video1 = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/zHPtvw8c3Lc?rel=0"
        frameborder="0"
        allowfullscreen
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        width="640" height="480"></iframe>
</div>
|]

segue = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>

Support for hot-swapping is live on this site, so you can [mess with Mario
yourself](/edit/examples/Intermediate/Mario.elm)
and play with the bouncing ball below. As you tweak the colors, shapes,
and physics in the code, you will see the ball update automatically.
|]

editor = [markdown|
<iframe src="/edit/examples/Intermediate/Bounce.elm?cols=100%25%2C150px"
        frameborder="0"
        style="overflow:hidden; height:400px; width:100%"
        height="400px" width="100%"></iframe>
|]

rest1 = [markdown|

<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
h2, h3, h4 {
  font-family: futura,'century gothic','twentieth century',calibri,verdana,helvetica,arial;
}
</style>

There are [many more examples](/examples/Intermediate.elm), so you can
continue to explore Elm and hot-swapping.

Perhaps the most interesting thing about adding hot-swapping to Elm was that it
was quite easy. It took about four days. In that time, it became very clear that
the practicality of supporting hot-swapping was directly related to the abstractions
(or lack-thereof) in the language. This has direct implications for the future of
Interactive Programming in languages like JavaScript, Clojure, Elm, etc.

## How hot-swapping works in Elm

Elm uses [signals](/learn/What-is-FRP.elm) to represent values as they
flow through the program. You can think of a signal as a value that changes
over time or as a stream of events. Every Elm program sets up a network for
processing these signals, called a signal graph.
The following video describes signal graphs in much more detail and begins to
show how they can be used for hot-swapping:

|]

video2 = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/FSdXiBLpErU?rel=0"
        frameborder="0"
        allowfullscreen
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        width="640" height="480"></iframe>
</div>
|]

rest2 = [markdown|

<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
h2, h3, h4 {
  font-family: futura,'century gothic','twentieth century',calibri,verdana,helvetica,arial;
}
pre {
  margin: 0 30px;
  padding: 4px 10px;
  border: solid 1px rgb(235,235,235);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: white; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: white; }
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

<span style="color:#999;">
For tons more details on signals and signal graphs, see
[this thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)
or [this paper](http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html).
</span>

Okay, so our signal graph is a bunch of nodes. Each node is associated with
some state and a pure function. To implement hot-swapping in Elm, you must:

 1. Compile the new program, resulting in a new signal graph.
 2. Copy the state over from the old signal graph.

That&rsquo;s it.
This ensures (1) that our functions have been fully updated and (2) that all
of the state has been preserved. The process is fairly simple because Elm
has language features let us *safely* make many simplifying assumptions.
But even with optimal language features, hot-swapping is still impossible
in some cases.

## The limits of hot-swapping

It is possible to change a program so much that it is no longer compatable
with previous versions. If we try to hot-swap with incompatable code, it will
lead to runtime errors. The programmer will be left wondering if their new
code introduced a bug or if it was just a case of bad hot-swapping. Perhaps
they start hunting for a bug that does not exist. Perhaps they ignore a bug
that *does* exist. This is not Interactive Programming, this is a buggy IDE.

To make hot-swapping reliable, we must know when programs are incompatable.
The more precise we can be, the more reliable hot-swapping can be.
Obvious examples of incompatabilies are:

* **The API has changed.** If the types of the arguments to a
  function change, it is no longer compatable with the rest of the program.

* **It is not possible to copy the old state into the new program.**
  Perhaps there are now many more values and it is unclear how they relate
  to our new program. Perhaps functions are tightly coupled with state, making
  it hard to change either independently.

The ability to diagnose and respond to these issues depends on the language
you are working with. We will now see how both cases can be addressed with
features like [static types](#static-types),
[immutability and purity](#immutability-and-purity),
and [predictable structure](#predictable-structure).

### Static Types

It is quite common to change an API. Modifying or extending existing functions
often requires it. For example, the following two functions conceptually do the
same thing:

```haskell
distance (x,y) = sqrt (x^2 + y^2)

distance point = sqrt (point.x^2 + point.y^2)
```

But these functions are not interchangable because their types do not match.
One works on tuples and the other on records. It is likely a couple other
functions would need to be modified to get everything working again.
In a dynamically typed language like JavaScript, deciding if it is a good idea
to try to hot-swap cannot be fully automated. *The programmer* must decide.
If we want true Interactive Programming, we need to do better than this.

In a statically typed language like Elm, all type errors are found
automatically. The hot-swapper can wait until the entire program type checks before
trying to swap the new code in. The hot-swapper can also check to see if the
types of the old state matches the types of the new state, ruling out a whole
class of hot-swapping-induced runtime errors. With static types, hot-swapping
can be safely automated to a much greater extent.

### Immutability and Purity

Mutable state permits a tight coupling between state and functions.
Suddenly part of our programs state lives in functions. We cannot
just copy state across; we must examine all old functions to see if
they have any hidden or shared state. From there we must figure out how
that hidden or shared state connects to our new program, which may not
always be possible.

Elm makes hot-swapping easy by having a very clean separation between state
and functions. When we copy over the old state, we know that there is no shared
or hidden state that we are missing. This is made possible by the following
two features:

* [Immutabile Data](http://en.wikipedia.org/wiki/Immutable_object) &ndash;
  data cannot be modified after creation
* [Pure Functions](http://en.wikipedia.org/wiki/Pure_function) &ndash;
  same arguments, same result

Both have already been very successful for concurrency in languages
like [Erlang](http://www.erlang.org/), one of the few languages that also
supports hot-swapping.

### Predictable Structure

In Elm, the structure of signal graphs is known as soon as the program starts
and does not change. Elm&rsquo;s static signal graphs are possible because
Elm does not permit signals-of-signals.

Many other frameworks for Functional Reactive Programming (FRP), particularly in
imperative languages, allow signal graphs to change over time. With
dynamic signal graphs, programmers can create and destroy nodes as they see fit.
Unfortunately, when the structure of the signal graph no longer matches the the
initial structure, hot-swapping becomes very difficult. How do you copy state from
the old signal graph to the new one when the graphs are not the same?

The issues with dynamic signal graphs are just a subset of the issues that come
up in languages without any support for FRP at all. When a program is just a
spaghetti soup of callbacks and shared state, figuring out how the initial program
relates to the current program must rely on much more complicated program analysis.
I am not saying it is impossible, but figuring this out would at least earn you a PhD.

## The future of Interactive Programming in Elm

Elm makes many language-level decisions that make hot-swapping easy
and reliable. Many of the toughest problems with hot-swapping are solved
or avoided when you have features such as static types, immutability,
purity, and static signal graphs. Yet even with those features, there are
still some open technical questions:

* Programmers can change the structure of a signal graph in their code.
  Can a hot-swap ever be performed when the signal graph changes? Perhaps in
  a limited subset of &ldquo;safe&rdquo; changes?

* Even in a pure language, it is possible to associate state with functions by using
  [continuation passing style](http://en.wikipedia.org/wiki/Continuation-passing_style) (CPS).
  This comes up in [the Automaton library](/docs/Automaton.elm), which is an alternate way
  to write reactive code. Is it possible to persist state *and* update functions when using CPS?

There are also some fun questions:

  * How can ideas some of the more extreme ideas from Learnable Programming
    make hot-swapping an even better experience?

  * How would Elm integrate with an IDE like LightTable that is already focused
    on making the tools for Interactive Programming?

Both of these questions are more on the IDE and tooling side of things.
Elm provides a solid foundation for hot-swapping *at the language level*,
and I am excited to see how it can be used for truly Interactive Programming.

|]
