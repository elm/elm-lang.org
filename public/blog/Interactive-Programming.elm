
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
  , width w video
  , section segue
  , width w editor
  , section rest ]

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

<br/>JavaScript proved that the development loop can be as short as refreshing
your browser. This is great for quickly iterating and testing an idea, making
programming more accessible and more fun.

This was the first step towards [Interactive Programming][ip]: the ability to
modify and interact with programs *while they are running*.
Articles like [Learnable Programming](http://worrydream.com/LearnableProgramming/)
explore the potential of Interactive Programming.
Projects like [LightTable](http://www.lighttable.com/) work on the tooling,
creating an IDE for Interactive Programming
Elm takes the next step, exploring what it means to be a *language* for
Interactive Programming. Elm&rsquo;s online editor now allows you to modify
running code, so you do not have to restart your program to change its behavior.

 [ip]: http://en.wikipedia.org/wiki/Interactive_programming

In Elm, this is called [hot-swapping](http://en.wikipedia.org/wiki/Hot_swapping).
Hot-swapping means *modifying running code*. Check out this video for a short demo:

|]

video = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/zHPtvw8c3Lc"
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

rest = [markdown|

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
the practicality of supporting Interactive Programming was directly related
to the abstractions (or lack-thereof) in the language.

## How hot-swapping works in Elm

Elm uses *signals* to represent values as they flow through the program.
An Elm program is basically a network for processing these values. This
event processing network is called a [signal graph][].

  [signal graph]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html

  VIDEO

Each node in the signal graph is associated with a pure function. When
we hot-swap, we need to replace these functions. Some nodes may also
hold state, keeping a record of the previous frame. This is what we
must preserve to maintain the existing state of our application.
To implement hot-swapping in Elm, one simply needs to compile the
new program and copy the state over from the old signal graph.

## Language features that make hot-swapping easy

Not all languages are going to work so well with Interactive Programming.
Hot-swapping was incredibly straightforward in Elm because the language has
strong abstractions that dramatically simplify the problem:

 * [Immutable Values](#immutable-values)
 * [Static Types](#static-types)
 * [Predictable Structure](#predictable-structure)

These language features make it easy to implement hot-swapping, but more
importantly, they make it easier for developers to *use* hot-swapping.
Let&rsquo;s examine these language features individually to see how each
provides concrete benefits.

### Immutable Values

[Immutability](http://en.wikipedia.org/wiki/Immutable_object)
means *values cannot be modified after creation*. You can
create new values, but you cannot change old ones. Immutability already
been very successful for concurrency in languages like
[Erlang](http://www.erlang.org/), one of the few languages
that also supports hot-swapping.

Limiting yourself to immutable values gives you a very important property:
*giving a function the same arguments **always** gives the same result*.
This is nice for testing and debugging, but it is a major benefit hot-swapping.
Immutability guarantees that you can simply swap in a function by name, the
internal details do not matter. With immutability, a function cannot have
shared state that must be migrated through the hot-swap.

With mutable values, hot-swapping must examine the details of each function
and find any state that must be preserved. It must also figure out how that
state relates to the state in the *new* function, which may not always be possible.
Figuring this out greatly complicates the process of swapping in code, and if
state is not preserved correctly it will lead to unexpected behavior.
The programmer will be left wondering if their no code introduced a bug
or if it was just a case of bad hot-swapping. Perhaps they start hunting
for a bug that does not exist. Perhaps they ignore a bug that *does* exist.

### Static Types

In some cases it is impossible to do a hot-swap with a running program.
If the data-structures used in the program change, the new functions and old
state become incompatible. For example, the following two functions are *not*
interchangeable even though they do the same thing:

```haskell

distance (x,y) = sqrt (x^2 + y^2)

distance point = sqrt (point.x^2 + point.y^2)
 
```

In a dynamically typed language like JavaScript, you would just swap the code and
*hope* the runtime error happens quickly and is easy to reproduce. The programmer
will be left wondering if their code was wrong or if there was a problem hot-swapping.
Perhaps the programmer starts chasing a bug that does not even exist. Perhaps
the programmer ignores a bug that *does* exist!

In a statically typed language like Elm, it is possible to find these problems
immediately and restart the program automatically. This guarantees that hot-swapping
cannot introduce runtime errors into a correct programming.

### Predictable Structure

In Elm, the structure of signal graphs is known as soon as the program starts
and does not change. Elm&rsquo;s static signal graphs are possible because
Elm does not permit signals-of-signals.

Many other FRP frameworks, particularly in imperative languages, allow signal
graphs to change over time. With these dynamic signal graphs, programmers can
create and destroy nodes as they see fit. Unfortunately, when the structure of
the signal graph no longer matches the the initial structure, hot-swapping becomes
very difficult. How do you copy state from the old signal graph to the new one when
the graphs are not the same? When there are now extra nodes that were not there
initially?

The issues with dynamic signal graphs are just a subset of the issues that come
up in languages without any support for FRP at all. When a program is just a
spaghetti soup of callbacks and shared state, figuring out how the initial program
relates to the current program must rely on much more complicated program analysis.
I am not saying it is impossible, but figuring this out would at least earn you a PhD.

## The future of Interactive Programming in Elm

Elm makes many language-level decisions that make Interactive Programming easy
to implement and pleasant to use, but hot-swapping in Elm is not perfect yet.
There are still some tough questions:

  * Can a hot-swap be performed when the signal graph changes? Perhaps in
    a limited subset of &ldquo;safe&rdquo; changes?
  * When the state that is persisted across a hot-swap is a function that uses
    [continuation passing style](http://en.wikipedia.org/wiki/Continuation-passing_style),
    is it necessary to be more clever about how hot-swapping works?

There are also some fun questions:

  * How can ideas some of the more extreme ideas from Learnable Programming
    make hot-swapping an even better experience?
  * How would Elm integrate with an IDE like LightTable that is already focused
    on making the tools for Interactive Programming?

Both of these questions are more on the IDE and tooling side of things.
Elm provides a solid foundation for Interactive Programming
*at the language level*, and I am excited to see how this can be
used to create a new kind of REPL.

|]
