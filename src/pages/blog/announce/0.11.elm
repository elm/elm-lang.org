import Blog
import Center


main =
  Blog.blog
    "Elm 0.11"
    "Drastically improved FFI"
    Blog.evan
    (Blog.Date 2014 1 21)
    [ Center.markdown "600px" content ]


content = """

The key idea for this release comes from a “component model” for using Elm in
production systems. A “component model” means you write small UI widgets or
processing units in Elm and [embed them in a traditional
project](/guide/interop) written in JS. So you can try
Elm out on a specific problem and see if it works for you.

<img src="/assets/blog/0.11/embed.png" alt="Component Model"
     style="width:480px; height:320px; margin-left: auto; margin-right: auto; display:block;">

To make this “component model” possible, Elm 0.11 introduces
[ports](http://guide.elm-lang.org/interop/javascript.html) which make it easy to
communicate between Elm and JavaScript. Check out
[how to embed Elm in HTML](/guide/interop) and
[how to communicate between Elm and JS](http://guide.elm-lang.org/interop/javascript.html)
to learn the specifics of these new features. This post will focus
on why ports are extremely important for Elm and how to understand
and use ports effectively.

## Importance of the “component model”

The success stories I have heard for JVM languages like Scala and Clojure
go something like this: “There was some module that was a terrible
quagmire of state and bugs and slowness. [There did not appear to be any
solution](http://youtu.be/WzEhoyXpqzQ?t=31s). An individual rewrote it in
language X. It was a third as long, ran faster, and was easier to maintain.
There was much rejoicing and language X became the primary language of that team.”
The key components of these stories are:

 * There was a known problem that seemed intractable.
 * The problem was bad but small enough for one person to fix.
 * Approaching the problem in a functional style drastically improved
   things in a way that was clear to people who make decisions.

It is not about being theoretically better, it is about being demonstrably
better. Getting a foothold requires running small experiments and finding
problems that are particularly well-suited to purity, immutability,
reactivity, type-safety, etc. The component model makes this kind of
story possible for Elm.

## Introduction to Ports

Here is a simple module `FinancialAdvisor` that uses ports to import stock
prices and export buy orders.

```elm
module FinancialAdvisor where

-- incoming signal of prices
port prices : Signal Float

-- outgoing signal of buy orders
port orders : Signal String
port orders = always "BRK.A" <~ prices
```

All ports are declared with the `port` keyword. Incoming ports are just a
name and a type annotation. In the example above, the `prices` port is
a signal of floats that will be sent in from JS. Outgoing ports require
a name, type annotation, and *definition*. A definition indicates that
the value should be sent out to JS. So the example above outlines a very
simple investment strategy:

 1. Get incoming stock prices.
 2. Ignore those stock prices.
 3. Buy [Berkshire Hathaway class A shares](https://www.google.com/finance?q=NYSE:BRK.A).
 4. Profit.

Now all that you need to do is initialize this winning investment strategy
from JS:

```javascript
// initialize the component, specifying all incoming ports
var advisor = Elm.worker(Elm.FinancialAdvisor, { prices:100 });

// set up code to follow to the advisor's orders
function buy(stock) { /* buy that stock */ }
advisor.ports.orders.subscribe(buy);

// send some prices to the component
advisor.ports.prices.send(103);
advisor.ports.prices.send(94);
advisor.ports.prices.send(99);

// decide that this is not a good investment strategy
advisor.ports.orders.unsubscribe(buy);
```

One could make this component more sophisticated by actually doing some
analysis of the incoming prices to guide the buy orders. That might be
tough to do well, but at least sending values between Elm and JavaScript
is pretty easy now!

More example uses of ports can be found in
[this document](/guide/interop),
[this complete example](https://github.com/evancz/elm-html-and-js), and
[this small example](https://gist.github.com/evancz/8521339).

## Customs and Border Protection

Ports must be careful about what values are allowed through.
Elm is statically typed, so each port is fitted with some
border protection code that ensures that type errors are kept
out. Ports also do some conversions so that you get nice
colloquial data structures in both Elm and JS.

The particular types that can be sent in and out of ports is
actually quite flexible. It covers pretty much [all valid JSON values](http://www.json.org/). Incoming
ports can handle [all JS values](https://github.com/elm-lang/elm-compiler/blob/0.11/libraries/JavaScript.elm)
and the following Elm types:

  * **Booleans and Strings** &ndash; both exist in Elm and JS!
  * **Numbers** &ndash; Elm ints and floats correspond to JS numbers
  * **Lists**   &ndash; correspond to JS arrays
  * **Tuples**  &ndash; correspond to fixed-length, mixed-type JS arrays
  * **Records** &ndash; correspond to JavaScript objects
  * **Signals** &ndash; correspond to event streams in JS
  * **Maybes**  &ndash; `Nothing` and `Just 42` correspond to `null` and `42` in JS

All conversions are symmetric and type safe. If someone tries to give a
badly typed value to Elm it will throw an error in JS immediately. By having
a border check like this, Elm code can continue to guarantee that you will
never have type errors at runtime.

Outgoing ports let you export all of the values listed above with
one important addition: first-order functions!
If you wrote a nice parser or library in Elm, you can use those functions
directly in JS. The mapping between Elm and JS function looks like this:

```elm
add x y = x + y
```
```javascript
function add(x,y) { return x + y; }
```

You lose currying on the JS side, but the goal of this whole feature is to
produce *colloquial* values in both Elm and JS. One important restriction on
exporting functions is that they must be *first-order* functions. Things
like `map` and `foldl` cannot be exported because the Elm compiler may
eventually perform optimizations that assume purity, and higher-order
functions allow you to introduce impure functions which *could* be executed
in an unexpected order. (Thanks to Max New for pointing this out!)

## Design goals and inspiration for Ports

Before this release, talking to JS was pretty intense and unwieldy. It required
lots of keywords, felt clunky, and was limited to signals. It was technically
possible to use Elm in a component model, but it just felt onerous.
So the biggest goal for this API was to make it easy and fun to
communicate between Elm and JS. The two biggest inspirations for ports were:

* **Message-passing concurrency**:
  [Concurrent ML](http://people.cs.uchicago.edu/~jhr/papers/2009/icfp-parallel-cml.pdf)
  was extremely influential in [the theoretical work underlying
  Elm](/assets/papers/concurrent-frp.pdf).
  The key abstraction from Concurrent ML is the *channels* which allow messages to
  be passed between components that run concurrently know nothing about each other (similar
  to Go and Erlang). I cannot say enough good things about this style of
  programming. Message-passing is the essense of ports and the component model,
  and I really hope it becomes more known and accepted in the JS community.

* **Flow-based Programming**: I was mostly inspired by the strong visual language
  of [flow-based programming](http://en.wikipedia.org/wiki/Flow-based_programming).
  Although the diagrams I see are usually [sad looking UML with randomly selected
  colors schemes](http://en.wikipedia.org/wiki/File:FBP_-_Simple_network.png), they
  make the core concepts of a “component model” extremely clear and have the *potential*
  to be beautiful. This is also where I got the term “port”.

I think these concepts give a good context for thinking about and using ports.
Although ports let you import and export non-signal values, the primary intent
of ports is to communicate via events and promote a “component model” for using
Elm.

## Other News

With more pull requests coming in, it is more important than ever to
develop good guidelines and strategies for making it fun and easy to
contribute to Elm. We are only beginning to address these issues, but
this release makes good progress:

Huge thank you to [Max New](https://github.com/maxsnew)
setting up tests for the compiler! Regressions were always extremely
rare, but with more contributors, having a good test suite is becoming
more and more important. Thank you to [Justin Leitgeb](https://github.com/jsl)
for making early contributions in this direction and getting Elm set
up with Travis CI. And thank you to Laszlo and Spiros for generally
giving me a hard time about testing!

We also started using the `-W` flag to catch a reasonable subset of silly
mistakes. A proper style guide is still needed, but this is a decent first
step.

## Thank you!

Thank you to [Max New](https://github.com/maxsnew) for fixing a bad corner
case with the `--make` flag and adding autocompletion to [Elm
REPL](https://github.com/elm-lang/elm-repl#elm-repl). Thank
you to [Joe Collard](https://github.com/jcollard/) for continuing to improve
[elm-mode](https://github.com/jcollard/elm-mode) for emacs, now [with REPL
integration](https://groups.google.com/forum/#!searchin/elm-discuss/elm-mode/elm-discuss/t82zCnn89Ps/5gLkvG9iE3EJ)
and super easy to install from [MELPA](http://melpa.milkbox.net/).

Thank you to [Joseph Hager](https://github.com/ajhager) and
[John P. Mayer](https://github.com/johnpmayer) for some nice bug fixes.

Finally, ports went through tons of iterations and brainstorms. At
every phase of these discussions, people had great comments and
guidance and ideas. We also tried *a lot* of different syntax, so
thanks to everyone who contributed to these discussions! It was very
helpful in making these decisions quickly.

"""
