import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


(=>) = (,)


main =
  Blog.blog
    "Time Travel made Easy"
    "Introducing Elm Reactor"
    Blog.michael
    (Blog.Date 2014 9 24)
    body


body =
  [ Center.markdown "600px" contentOne
  , iframe
        [ src "http://debug.elm-lang.org/edit/Thwomp.elm"
        , style [ "border" => "none", "overflow" => "hidden", "height" => "400px", "width" => "80%", "padding" => "0 10%" ]
        ]
        []
  , Center.markdown "600px" contentTwo
  ]


contentOne = """

Elm Reactor grew out of my internship working on Elm at Prezi this summer. It
improves the [time traveling debugger][debug] created by Laszlo Pandy and Evan
Czaplicki, turning it into a practical development tool. It has more features,
a nice new UI written in Elm, and can now be used with *any* text editor. Elm
Reactor is distributed with [Elm Platform 0.13][13], so it is easy to
[install][] and use right now.

[debug]: http://debug.elm-lang.org/
[13]: /blog/announce/0.13
[install]: /install

Check out the following video to see Elm Reactor in action when debugging a
[TodoMVC app][todo] written with [elm-html][html]:

[todo]: https://github.com/evancz/elm-todomvc/blob/master/Todo.elm
[html]: /blog/blazing-fast-html

<div class="intrinsic-container">
<iframe allowfullscreen
        src="//www.youtube.com/embed/2HK4ENBPcWA?rel=0&html5=1"></iframe>
</div>
<div style="text-align: right; color: #D8DDE1; padding-top: 4px; font-size: 0.5em;">Videos Narrated by Evan Czaplicki</div>

# Ultimate Undo Button

Elm Reactor lets you travel back in time. You can pause the execution of your
program, rewind to any earlier point, and start running again. Watch me misplace
a line piece and correct my mistake when playing [elmtris][]:

[elmtris]: https://github.com/jcollard/elmtris

<div class="intrinsic-container">
<iframe allowfullscreen
        src="//www.youtube.com/embed/IwOka_IXjU4?rel=0&html5=1"></iframe>
</div>

In this example, I paused the game, went back, and continued to avoid crushing
defeat. This is what &ldquo;time traveling&rdquo; means in Elm Reactor. It lets
you:

* Pause a running program
* Step backwards and forwards in time
* Continue from any point in the program&rsquo;s past

This sort of time traveling lets you explore the interaction space of your
program faster. Imagine debugging an online checkout page. You want to
verify that the error messages look right. There are several dozen
ways to trigger an error message (e.g., bad phone number, no last name, etc.).
Traditionally you would need to repeat the entire transaction for each error,
slowly going crazy as you re-enter the same data for the 13th time. Elm
Reactor lets you rewind to any point, making it easy to explore an alternate
interaction. The next few sections will describe how Elm Reactor makes this
possible.

### Recording Inputs

All input sources to an Elm program are managed by the runtime and known
statically at compile-time. You declare that your game will be expecting
keypresses and mouse clicks. This makes the inputs easy to track. The first step
in time traveling is to know your history, so Elm Reactor records these input
events.

The next step is to pause time. The following diagram shows how an event such as
a keypress or mouse click comes to the Elm runtime. When an event happens, it is
shown on the &ldquo;Real Time&rdquo; graph and when your program receives the
event, it is shown on the &ldquo;Elm Time&rdquo;. When Elm Reactor pauses Elm,
the program stops receiving inputs from the real world until Elm is unpaused.

<img
  src="/assets/blog/time-travel-made-easy/timeline-pause.png"
  alt="pausing time">

Events in Elm have a time associated with them. So that Elm does not get a hole
in its perception of time, Elm Reactor offsets that recorded time by the time
spent paused. The combination of event value and time means that these events
can be replayed at any speed (read: really fast).

### Safe Replay

Elm functions are pure, meaning they don&rsquo;t write to files, mutate state,
or have other side-effects. Since they don&rsquo;t modify the world, functions
are free to be replayed, without restriction.

Elm programs may have state, even though all functions are pure. The runtime
stores this state, not your program. The input events dictate how the state will
change when your program is running. Elm Reactor can jump to any state because
this internal state is determined entirely by the recorded input events. The Elm
runtime combines the previous state and new inputs to make the current state.
So, to jump to any point in time and have a sensible state, you must replay the
events leading up to that point.

The simple approach to time travel is to start from the beginning and replay
everything up to the desired event. So if you wanted to step to the 1000th
event, you would have to replay 1000 events. Elm Reactor uses *snapshotting* to
avoid replaying so many events.

### Snapshotting

Snapshotting is to save the state of your application in a way that can be
restored. Elm&rsquo;s version of [FRP](/learn/What-is-FRP.elm) makes this
straightforward and cheap. There is a clean separation of code and data: the
application data is totally separate from the runtime. So to snapshot an Elm
application we only have to save the **application data** and not implementation
details like the state of the stack, heap, or current line number. This is most
equivalent to saving the model in MVC.

Elm Reactor takes a snapshot every 100 events. This means jumping to any event
from any other takes no more than 100 event replays. For example, to jump to
event 199 from event 1000 Elm Reactor first restores the snapshot at event
100, then applies the next 99 recorded events. A better user experience
strategy to snapshotting could ensure time travel never takes more than N
milliseconds. This could be done by timing each round of computation and
snapshotting every N milliseconds. Instead Elm Reactor uses the simpler
snapshot-every-Nth strategy for its initial release.

# Changing History

In addition to time travel, Elm Reactor lets you change history. Since
Elm Reactor records the entire history of inputs to the program, we can simply
replay these inputs on new code to see a bug fix or watch how things change.

<div class="intrinsic-container">
<iframe allowfullscreen
        src="//www.youtube.com/embed/RPNxNAJG4EU?rel=0&html5=1"></iframe>
</div>

In this example, Mario&rsquo;s image URL and gravity were set incorrectly. Mario
had already made a few jumps and time had passed. But the functions that control
Mario could be swapped out because the functions are independent from their
inputs. So despite having played with Mario, Elm Reactor can still swap in new code.

Playing a game while you build it is quite nice, but this is also remarkably handy
for more typical applications. In the checkout example we described earlier,
perhaps the last screen misplaced a close button. Once you navigate to that
page, Elm Reactor lets you mess with the code as much as you want while you find
the right place for the close button. You can see the results of your new code
without maddeningly running through the entire interaction each time!

But what happens when you try to swap in a program with a type error or syntax
error? In that case, Elm Reactor does not swap in the new code. Instead, it
displays a message explaining the issue while the last working version keeps
running. The following video shows this kind of feedback:

<div class="intrinsic-container">
<iframe allowfullscreen
        src="//www.youtube.com/embed/xlP-Bpdv1lc?rel=0&html5=1"></iframe>
</div>

# Try it yourself!

The editor below lets you try out all of the features described so far. If you
click on the tab of the debugger panel it will slide away, showing more of
Thwomp. Try it out!

"""


contentTwo = """

This is running at [debug.elm-lang.org](http://debug.elm-lang.org) where there
are some other examples you can explore, such as [the Mario example][mario]
where you can play with physics and [a simple stamping app][stamp] where it is
fun to change the size and shape of each stamp.

[mario]: http://debug.elm-lang.org/edit/Mario.elm
[stamp]: http://debug.elm-lang.org/edit/Stamps.elm

# In your workflow

Elm Reactor will work with any pure Elm project. Use it with  [elm-html][],
[elm-webgl][], [elm-d3][], or any other renderer.

[elm-html]: /blog/blazing-fast-html
[elm-webgl]: /blog/announce/0.12.3
[elm-d3]: https://github.com/seliopou/elm-d3

<img
  style="width:200px; height:100px;"
  alt="html"
  src="/assets/blog/time-travel-made-easy/elm-html.png">
<img
  style="width:190px; height:100px;"
  alt="webgl"
  src="/assets/blog/time-travel-made-easy/elm-webgl.png">
<img
  style="width:200px; height:100px;"
  alt="d3"
  src="/assets/blog/time-travel-made-easy/elm-d3.png">

Elm Reactor can also integrate with your favorite editor. The code
swapping is editor-agnostic; it just watches your project directory for file
saves. There is no need for an emacs, Sublime Text, or vim plug-in. It just
works!

That applies for multi-module projects, too! Whenever Elm Reactor detects a file
change, it tries to recompile the main Elm file, which recompiles any
dependencies. For more information about using the debugger in your own
workflow, check out the [repository](https://github.com/elm-lang/elm-reactor).


# What&rsquo;s next

This is the first public release of Elm Reactor. There are many useful ideas
and plans that didn&rsquo;t make it in the first version. The long-term vision
includes:

* **REPL in the Reactor** - A [read-eval-print loop][repl] (REPL) is super
useful for testing specific functions in a large project. Imagine an in-browser
REPL that knows about your code so you can explore an idea in a scratchpad or
make sure a function does what you expect.

[repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

* **Ports** - [Ports][] make it easy for Elm programs to send messages to and
from JS, where there may be all sorts of side-effects. It is not immediately
obvious how ports will work with time travel. The easy solution is to never
send values out of ports, avoiding unwanted side-effects like writing to disk
many times as we rewind. Some ports are safe to replay though, so maybe more
nuance is needed.

[ports]: /guide/interop#ports

* **Hot-swapping** - Hot-swapping lets you keep the current state of a program
but change its future behavior. Some fast-paced Elm games may want this feature
to avoid the cost of replaying all events when swapping code. This has
already been implemented for Elm [as described here][hotswap], so the technical
part should be straight-forward.

[hotswap]: /blog/interactive-programming

* **Save Event Sequences** - Elm Reactor already saves inputs to a program. If
you could give these inputs to someone else, you could easily file an
informative bug report that shows *exactly* how to reproduce an error.

* **Improved visualizations** - It may help to use techniques like
[sparklines](http://en.wikipedia.org/wiki/Sparkline) to visualize
tracked values (e.g., mouse position).

* **Plug-ins** - Elm Reactor may be a nice way to expose lots of functionality
in browsers with a nice UI. A plug-in system would make it easier for the Elm
community to make editor-agnostic tools.

There are a lot of great ideas that can make Elm Reactor even more
powerful. If you&rsquo;re interested, check out
[the repository](https://github.com/elm-lang/elm-reactor/)
and the [community][]. We will be happy to help get you
started on a project.

[community]: https://groups.google.com/forum/#!forum/elm-discuss

# Thank You

Thank you [Evan Czaplicki][evan] for your guidance, wisdom, and patience while
writing this. You taught me an astonishing amount about FRP and rigour this
summer. I&rsquo;m so grateful for the methodologies I picked up from you.

[evan]: https://twitter.com/czaplic

Thank you to [Laszlo Pandy][laszlo] who demonstrated the possibility of
debugging like this by writing the prototype Elm debugger. Thanks [Gábor
Hoffer][gabor] and the Prezi design team for the suggestions on making the
debugging tab pretty!

[laszlo]: https://github.com/laszlopandy
[gabor]: https://twitter.com/hoffergabor

Thank you to Bret Victor, whose talk,
[&ldquo;Inventing on Principle&rdquo;](https://www.youtube.com/watch?v=PUv66718DII) offered
valuable insight into what the debugging experience should be like.

"""
