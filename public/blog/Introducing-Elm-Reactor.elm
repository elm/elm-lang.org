import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Introducing: Elm-Reactor"

main = lift (skeleton "Blog" everything) Window.dimensions

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w pageTitle
  , section author
  , section content
  , width w example
  , section closing
  ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Introducing Elm Reactor</div>
<div style="font-size: 1.5em;">An Interactive Programming Tool</div>
</div>
|]

author = [markdown|
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center; font-size: 1em; text-align: right; font-style: italic;">
By <a href="http://github.com/michaelbjames">Michael James</a>
</div>
|]

content = [markdown|

<br/>

Elm has a new development tool, the Reactor. It combines several recent features
of the language into a kickass debugging tool. Thanks to Elm’s unique design,
the Reactor lets you:

* Swap running code with an update
* Go back to any previous state of the program
* Develop and debug with any Elm program (even Elm-HTML)

This new tool sits at the center of the development cycle. It builds code and
shows the results with the ability to debug those results. It merges the
prototype time-traveling debugger with rapid development features
like automatic hot-swapping.

<img src="/imgs/reactor-post/fold_small.gif" style="width:100%" height="486px">

# Elm-Reactor

The Reactor is your tool for developing Elm programs. It wants you to stop
focusing on the middle steps surrounding your code and just write cool stuff.
It’ll take care of the intermediate stuff.

## Interactive Programming by Default

A delay between updating the program and seeing the effects slows down the
entire development process. **Interactive programming is programming with
immediate feedback**. Interactive programming connects the developer to the
changes made as soon as they’re made.

Hot-swapping is changing live code with new code. Elm uses this technique for
interactive programming. The runtime permits new functions and values to **swap
in while the program is running**.

<img src="/imgs/reactor-post/mario_small.gif" style="width:100%" height="451px">

In [this example](http://debug.elm-lang.org/edit/Mario.elm), Mario’s image URL
and gravity was incorrect, but he had already made a few jumps. The functions
that control Mario can be swapped but still use the state that Elm’s signal
graph carries about Mario’s location and past.

The piping that guides inputs through the program’s functions&mdash;the signal
graph&mdash; holds the state of the program. An interactive program may use a
`foldp` to carry state from one input update to the next. So when the code
changes, the functions and values update but the state remains as long as the
program structure remains the same. All the previous inputs (Mario jumping and
time passing) are applied to the new code and Mario appears, with the same
history has before the hot-swap.

With automatic hot-swapping, we’re closing the feedback loop. Programming with
Elm's Reactor is as interactive as your save-habits. For more information on how
Elm can hot-swap code, read the
[post about it](http://elm-lang.org/blog/Interactive-Programming.elm).


## Changing history

On its own, hot-swapping will make development faster. But Elm can do more to
help the programmer. Elm's design supports time traveling debugging, and Laszlo
Pandy made a prototype last year. Time traveling debugging is two things:

* Stepping back in time of a program’s execution.
* Continuing from any point in the program’s past, to make a new future.

The Reactor realizes this possibility. Watch me travel back in time to not lose
in Tetris.

<img src="/imgs/reactor-post/tetris_small.gif" style="width:100%" height="485px">

In this example, we can investigate the series of events that led up to my
defeat. The Reactor will let you inspect every point in the program. It is
even possible to **change the outcome of a program** by using new events or
changing code with a hot-swap.

Going back in time for an Elm program does not have to be expensive. It does not
have to store the state of the world every step of the program. Instead it only
has to store what happened (e.g., the ‘b’ button was pressed, the mouse moved to
another coordinate, etc.). The state of the program is a just a certain flow of
this data, for two reasons:

* **Separation of code and data** - All program state is held in the signal
  graph&mdash;the plumbing that moves inputs to outputs through the program’s
  functions. Some of the plumbing accumulates state when the program uses a past-
  dependent function. The code inside these functions is separated entirely from
  the data they work with. So it’s possible to replay old inputs on the functions,
  swap the functions, or clear the data!

* **Functional Reactive Style** - Input from the outside world (e.g., screen
  size, mouse position, keypresses) flow through the pure functions to make a
  new frame.

To go back in time Elm must only record the inputs from the world and just
reapply the functions as necessary. To read more about time traveling in Elm,
check out the [original blog post](http://debug.elm-lang.org/)!


## Compatible with the Elm-HTML

The reactor is not restricted to vanilla Elm. Debugging complex user
interactions was a motivating factor in the reactor’s creation. Naturally, Elm-
Reactor works with Elm-HTML. **Blazing fast HTML can be accompanied by
time-traveling development tools**. You are free to use your own CSS, your own
javascript, and your own HTML.

<img src="/imgs/reactor-post/html_small.gif" style="width:100%" height="403px">

The Reactor can be attached to any Elm program by including two lines of
code. To attach the debugging panel to your elm program simply import the
debugging script in your final html file and start Elm in debugging mode:

```
<script type="text/javascript" src="/debugger.js"></script>
var runningElmModule = Elm.debugFullscreen(Elm.Todo,"path/to/todo.elm");
```

For more information about using the debugger in your own workflow, check out
the [repository](https://github.com/elm-lang/elm-reactor).


## Big Projects

Complex projects aren’t contained in just one file. There are several files but
ultimately one file brings them all together. So when any of the dependent files
change, the main file gets recompiled which sends a hot-swap of the entire
project. State is maintained throughout the running program. Elm's Reactor is
ready to help create impressive and complex programs.


## Editor Agnostic

The Reactor works on a level below text editors to give a developer an
interactive programming experience. It listens for changes on files to
know when to send a hot-swap update to the browser. It’s not a new feature for
the world but it’s certainly a handy one.

What about when you make a syntax error? The browser gives you the error but
the Reactor doesn’t try to swap the running code with bad code.

<img src="/imgs/reactor-post/error_small.gif" style="width:100%" height="362px">

Instead, the old version keeps working and will get swapped for a newer version
only when the new version compiles.

# Try it yourself!

You can hide the debugging panel by clicking on the tab.
|]

example = [markdown|
<iframe src="http://debug.elm-lang.org/edit/Thwomp.elm?cols=100%25%2C150px" frameborder="0" style="overflow:hidden; height:400px; width:100%" height="400px" width="100%"></iframe>
|]

closing = [markdown|
# What's next

Elm's Reactor is meant to build applications for lots of people to see. It
is a capable tool for building a complex web app. The project is far from
complete, though. There are a few ideas that will certainly make Elm development
easier and I want to see them happen.

* **Import & Export** - The next step in the development of the reactor is to permit the
import and export of events. It means bug reports could include the exact (and
entirely reproducible) series of events that led up to the bug.

* **Benchmarking** - When we can import and export event series we’ll be able to
cleanly feed them into a future revision of the Elm benchmarker. We’ll be able
to compare elm's speed with a couple mouse clicks!

* **REPL in the Reactor** - The REPL is super useful for development. I imagine a
supped-up in-browser REPL that is aware of your code so you can try out ideas in
a scratchpad before putting them in your text editor.

* **Graph Watches** - Wouldn’t it be helpful to see a graph
of how Mario’s velocityhas changed over time (e.g., a
[sparkline](http://en.wikipedia.org/wiki/Sparkline))? With experimentation and
thought, we can find the right visualization for other types of data (e.g.,
Strings, ADTs, etc.).

* **Plugins** - What if you could add whatever you wanted to the Reactor? Plugins are important to making development easier. The growing community will surely have better ideas about what they want in their own development cycle and will want to contribute.

There are a lot of great ideas that can go into making the reactor even more
powerful. If you’re interested, check out
[the repository](https://github.com/elm-lang/elm-reactor/)
and the community. We will be happy to help get you
started on a project.


# Thank You

Thank you Evan Czaplicki for the guidance and wisdom while writing this. You
taught me an unreal amount about FRP and everything else this summer, I’m so
grateful to have had this experience.

Thanks Prezi Design Team for the suggestions on making the debugging tab pretty!
Thank you to Laszlo Pandy who wrote the prototype Elm debugger. He demonstrated
the possibility of debugging like this.

Thank you to Bret Victor whose talk,
[“Inventing on Principle”](https://www.youtube.com/watch?v=PUv66718DII) offered
inspiration on interactions and was game Laszlo the idea for his debugger.

|]
