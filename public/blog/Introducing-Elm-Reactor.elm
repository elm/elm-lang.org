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
  , section content
  , width w example
  , section workflow
  , section closing
  ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Introducing Elm Reactor</div>
<div style="font-size: 1.5em;">An Interactive Programming Tool</div>
</div>
|]

content = [markdown|

<p style="text-align: right;">
By <a href="http://github.com/michaelbjames">Michael James</a>
</p>

Elm Reactor is a development tool that gives you the power to time-travel.
Pause, rewind, and unpause any Elm program to find bugs and explore the
interaction space. Elm Reactor swaps in new code upon saving, letting you know
immediately if your bug fix or feature works. This way it&rsquo;s easy to tweak
layout, play with colors, and quickly explore ideas. Elm has prototyped [these
pieces before](http://debug.elm-lang.org/), but **Elm Reactor polishes them in
an [easy to install package][install]** that can be used with any text editor so
you can start using it for your projects today.

[install]: https://github.com/elm-lang/elm-platform#elm-platform

The following demo shows someone fixing a bug in a TodoMVC app written with
[elm-html](/blog/Blazing-Fast-Html.em). “Active Tasks” should filter tasks
marked as complete. Watch them find the bug, fix the code, see the fix propagate
automatically, and rewind the program to verify the fix.

<img src="/imgs/reactor-post/fold.gif" style="width:600px; height:364px;">

Elm Reactor grew out of my internship working on Elm at Prezi this summer. It
combines the time-traveling debugger prototype created by Laszlo Pandy and Evan
Czaplicki along with the modular design of Elm to make a practical development
tool. It harnesses the recent features of Elm to give the debugging process a
much needed upgrade.

# Ultimate Undo Button

Elm Reactor lets you travel back in time. You can pause the execution of your
program, rewind to any earlier point, and start running again. Watch me misplace
a line piece and correct my mistake:

<img src="/imgs/reactor-post/tetris.gif" style="width:600px; height:306px;">

In this example, I paused the game, went back, and continued to avoid crushing
defeat.  This is what I mean by “time-traveling” in Elm Reactor; to be more
formal “time-traveling” means to:


* Pause a running program
* Step back in time
* Continue from any point in the program&rsquo;s past to make a new future

This sort of time traveling lets you explore the interaction space of your
program faster. Imagine debugging an online checkout page with this. You need to
verify that the error messages look right. Now imagine there are several dozen
ways to reach an error message (e.g., bad phone number, no last name, etc.).
Traditionally you would need to repeat the entire transaction for each error
error, slowly driving you crazy as you mistype something for the 13th time. Elm
Reactor lets you rewind to any point making it easy to explore an alternate
interaction. Here&rsquo;s how Elm Reactor does it.

### Recording Inputs

All input sources to an Elm program are managed by the runtime and known
statically at compile-time. So you declare that your game will be expecting
keypresses and mouse clicks. This makes the inputs easy to track. The first step
to time-traveling is to know your history, so Elm Reactor records these input
events.

The next step to pause time. The following diagram shows how events such as
keypresses and mouse clicks come to the Elm runtime. The events when they come
in are shown on the &rdquo;Real Time&ldquo; graph and the events Elm sees are on
the &rdquo;Elm Time&ldquo; graph. When Elm Reactor pauses Elm the program stops
receiving inputs from the real world until Elm is unpaused.


<img src="/imgs/reactor-post/timeline-pause.png" style="width:600px; height:200px;">

Events in Elm have a time associated with them. So that Elm does not get a hole
in its perception of time, Elm Reactor offsets that recorded time by the time
spent paused. The combination of event value and time means that these events
can be replayed at any speed (read: really fast).

### Safe Replay

Elm functions are pure, meaning they don&rsquo;t write to files, mutate state,
or have other side-effects. Since they don&rsquo;t modify the world, it&rsquo;s
safe to replay events as many times as we like. Some inputs to an Elm program
can have state (e.g., the number of times the mouse clicked). This state can be
stepped only forward with an event (i.e., a mouse click) and there is no stepping
back. So to travel to a given point in time, you can simply replay events up to
that point.

The simple approach to time-traveling is to start from the beginning and replay
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
from any other takes no more than 100 event replays. A better user experience
strategy to snapshotting could ensure time-traveling never takes more than N
milliseconds. This could be done by timing how long each round of computation
takes to process an input and snapshotting every N milliseconds. Instead Elm
Reactor uses a simpler snapshot-every-Nth strategy for its initial release.

# Changing History

In addition to time-traveling, Elm Reactor also lets you change history. Since
the Reactor records the entire history of inputs to the program, we can simply
replay these inputs on new code to see a bug fix or how things change.

<img src="/imgs/reactor-post/swap.gif" style="width:600px; height:364px;">

In this example, Mario&rsquo;s image URL and gravity were set incorrectly. Mario
had already made a few jumps and time had passed. But since the program&rsquo;s
input history is separate from its functions, the latter could be swapped out.
Even better, the live code can be changed and the input events can be replayed
to see the effect of the changes!

Building a game while you play it is one thing but this is also remarkably handy
for more typical applications. In the checkout example we described earlier,
perhaps the last screen is missing a close button. Once you navigate to that
page, Elm Reactor lets you mess with the code as much as you want while you find
the right place for the close button. You can see the results of your new code
without maddeningly running through the entire interaction each time!

In real life, it&rsquo;s easy to get time-traveling wrong. People are always
disappearing from photographs and kissing grandparents. Elm Reactor will only
swap in *valid* programs. If there is a type error or syntax error, then the
program is not swapped. Instead an error message is displayed explaining the
issue and the last working version keeps running.

<img src="/imgs/reactor-post/error.gif" style="width:600px; height:364px;">

# Try it yourself!

You can hide the debugging panel by clicking on the tab.
|]

example = [markdown|
<iframe src="http://debug.elm-lang.org:1234/edit/Thwomp.elm?cols=100%25%2C150px" frameborder="0" style="overflow:hidden; height:400px; width:100%" height="400px" width="100%"></iframe>
|]

workflow = [markdown|

# In your workflow

Elm Reactor will work with any Elm project. Use it with  [elm-html][],
[elm-webgl][], [elm-d3][], or any other renderer. **If it&rsquo;s Elm it will work
with the Reactor**

[elm-html]: /blog/Blazing-Fast-Html.elm
[elm-webgl]: /blog/announce/0.12.3.elm
[elm-d3]: https://github.com/seliopou/elm-d3

<img style="width:200px; height:100px;" src="/imgs/reactor-post/elm-html.png">
<img style="width:190px; height:100px;" src="/imgs/reactor-post/elm-webgl.png">
<img style="width:200px; height:100px;" src="/imgs/reactor-post/elm-d3.png">

The Reactor can also integrate with your favorite editor. Even better the code
swapping is editor-agnostic; it just watches your project directory for file
saves. There is no need for an emacs, Sublime Text, or vim plug-in. It just
works.

That applies for multi-module projects, too! Whenever Elm Reactor detects a file
change, it will try to recompile the main Elm file which will recompile and
dependencies.

For more information about using the debugger in your own workflow, check out
the [repository](https://github.com/elm-lang/elm-reactor).|]

closing = [markdown|
# What&rsquo;s next

This is the first public release of Elm-Reactor. There are a lot of useful ideas
and plans that didn&rsquo;t make in the first version. The long-term vision includes:

* **REPL in the Reactor** - A [read-eval-print loop][repl] (REPL) is super
useful for testing specific functions in a large project. Imagine an in-browser
REPL that knows about your code so you can explore an idea in a scratchpad or
make sure a function does what you expect.

[repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

* **Ports** - [Ports][] make it easy for Elm programs to send messages to and
from JS, where there may be all sorts of side-effects. It is not immediately
obvious how ports will work with time-traveling. The easy solution is to never
send values out of ports, avoiding unwanted side-effects like appending too many
times as we rewind. But *some* ports are safe to replay.

[ports]: /learn/Ports.elm

* **Hot-swapping** - Hot-swapping lets you keep the current state of a program
but change its future behavior. Some fast-paced Elm games may want this feature
to avoid paying the cost of replaying all events when swapping code. This has
already been implemented for Elm [as described here][hotswap], so the technical
part should be straight-forward.

[hotswap]: /blog/Interactive-Programming.elm

* **Save Event Sequences** - Elm Reactor already saves inputs to a program. If
you could give these inputs to someone else you could file a perfect bug report
that shows *exactly* how to reproduce an error.

* **Improved visualizations** - It may help to use techniques like
[sparklines](http://en.wikipedia.org/wiki/Sparkline) to show
`watch` data more clearly.

* **Plug-ins** - Elm Reactor may be a nice way to expose lots of functionality
in browsers with a nice UI. A plug-in system would make it easier for the Elm
community to make editor-agnostic tools.

There are a lot of great ideas that can go into making the reactor even more
powerful. If you&rsquo;re interested, check out
[the repository](https://github.com/elm-lang/elm-reactor/)
and the community. We will be happy to help get you
started on a project.


# Thank You

Thank you Evan Czaplicki for the guidance and wisdom while writing this. You
taught me an unreal amount about FRP and everything else this summer, I&rsquo;m so
grateful to have had this experience.

Thanks Gábor Hoffer and the Prezi design team for the suggestions on making the
debugging tab pretty! Thank you to Laszlo Pandy who wrote the prototype Elm
debugger. He demonstrated the possibility of debugging like this.

Thank you to Bret Victor whose talk,
[&ldquo;Inventing on Principle&rdquo;](https://www.youtube.com/watch?v=PUv66718DII) offered
valuable insight into what this experience should be like.

|]
