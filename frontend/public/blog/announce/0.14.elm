import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Website.Tiles as Tile
import Window

port title : String
port title = "Elm 0.14"

main = skeleton "Blog" everything <~ Window.dimensions

everything wid =
    let w = min 600 wid
    in  flow down
        [ width w content
        ]

content = Markdown.toElement """

<br>

# Elm 0.14

This release has two major goals:

  * **Simplify the language and core libraries**<br>
    The signal library is much simpler, working with JSON and randomness is
    great, and we made types easier to learn and understand.

  * **Greatly improve the package manager and build tools**<br>
    We now have parallel builds and the beginnings of an innovative
    approach to package management.

I am really excited about this release. If you have been eying Elm from afar,
now is a great time to start taking a closer look. It feels like everything
is coming together. My goal has always been to make web programming pleasant,
but I never thought it would be quite this nice.

If you are in a rush or just want references to help you upgrade, check out
[the changelog], [elm-package], and [elm-make].
This post dives into the most exciting changes, showing how they connect to the
broader philosophy that guides all of the improvements in 0.14.

## Guiding Philosophy

A great teacher takes an idea and makes you *feel* it. They make it exciting
and alive. All the bad explanations you have heard before melt away, and you
feel the rush of comprehension.

I think [Elm Reactor]() embodies this. I think the [online editor] and [examples]
embody this. I want to design Elm such that this is happening in syntax and
libraries. As much as possible, I want people to see some code for the first
time and *feel* how it works. I want to bring the learning curve down from days
to minutes.


### Philosophy in Action

I write quite a lot of JavaScript and Elm on a daily basis, and I am excited
about Elm because it makes things so much simpler for me. I *feel* that
simplicity. People can make fancy arguments and talk about cool features all
day long, but the real challenge is to share that *feeling*.

It is obviously important to create delightful applications and excellent tools,
but in addition to that, we need to change how we talk. Terms like Algebraic
Data Type are hurting us. We are making useful ideas sound boring and confusing.
If my goal is to make a great user experience, what do I care about Algebra or
Types? It sounds like Data just got a lot more complicated, but how is that
making my users happier? Terms like this distract people from extremely useful
ideas. In the worst cases, the terminology actively alienates and discourages
people, so even when someone comes around with a good explanation it is too late.

This is not some pet theory I formed in a vaccuum. Between teaching functional
programming, fielding questions on the Elm mailing list and at conferences,
running an Elm meetup in SF, and just chatting with other programmers, I talk
to quite a lot of programmers in any given week. In all these cases, I find
people with different backgrounds and perspectives and talk through an
idea with them, always trying out different teaching strategies to see what
works and who it works for. If you have talked with me in the last year, you
probably contributed some data. This release is the first big step towards the
successful strategies. By changing some core terms, I hope we can begin to
become better teachers and story-tellers.


## Making Types Easier

### Type Aliases

The new type alias syntax looks like this:

```haskell
type alias Point = { x:Float, y:Float }
```

Without knowing anything about Elm, it is not a big stretch to see that there
is a type alias called `Point` and it is equal to something with an X and a Y.

### Union Types

We are introducing the term [Union Type][union] to refer to
&ldquo;putting together a bunch of different types.&rdquo; For example, maybe
your company has user IDs, and at first they were all integers, but later you
realized that integers are not big enough and had to switch to strings. You
might find yourself using a union type to represent this user ID:

```haskell
type UserID = OldID Int | NewID String

newID : UserID -> String
newID userID =
    case userID of
      OldID number -> toString number
      NewID string -> string
```

You would probably not believe how many times I found strangers and presented
this idea a bunch of different ways, but this is the result!

The goal is that someone can read the term &ldquo;union type&rdquo; randomly on
some forum or hear it in a conversation and have a pretty good idea what it is.
If you want to be extra precise, the term [tagged union][tagged] can be
helpful. Comparing union types to Java-style enumerations can also be successful
depending on who you are talking to.

Even with the best terminology, it can still be tough to give a good
explanation. For people who want to *teach* this concept, I written up
[a document][gist] that attempts to handle common questions gracefully. I have
also written up [a full description][union] that explains what they are and
shows a bunch of examples.

[union]: /learn/Union-Types.elm
[gist]: https://gist.github.com/evancz/06fe634245a3aab4a61b
[tagged]: http://en.wikipedia.org/wiki/Tagged_union


### List Types

The special syntax for lists has been removed. When you want to write the type
of a list function, now it is something like this:

```haskell
length : List a -> Int
```

The primary benefit here is consistency. People learning Elm do not need to
learn this one special case and cannot be led to think that there is something
extra special going on there. It also makes it easier to switch annotations
between `List`, `Set`, and `Array` depending on what you want to do. Finally,
it frees up the `[]` syntax in types just in case that could come in handy
some day.


## Making Signals Easier

### No More Lifting

The term `lift` is dead. It makes me a bit sad for my [thesis][], but I think
it will help a lot of people get started with signals more quickly. The new
term is `map`, and the goal is to build on the intuition people have from
working with lists.

[thesis]: /papers/concurrent-frp.pdf

```haskell
Signal.map  : (a -> b) -> Signal a -> Signal b
Signal.map2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
```

To make this connection stronger, the `List` library has changed a bit too.
Instead of having a bunch of `zip` and `zipWith` functions, everything has
become a variation of `map`. When you want to put many lists together,
combining values pairwise, you use the `map2` function.

```haskell
List.map  : (a -> b) -> List a -> List b
List.map2 : (a -> b -> c) -> List a -> List b -> List c
```

So if you want to put two lists together, you write expressions like this:

```haskell
List.map2 (,) [1,2,3] [1,2,3] == [(1,1), (2,2), (3,3)]
List.map2 (+) [1,2,3] [1,2,3] == [2,4,6]
```

These changes are paralleled in the Signal library, where the map functions all
work exactly the same way. This naming scheme is more in line with
[Clojure](https://clojuredocs.org/clojure.core/map),
[Racket](http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29),
and [OCaml](http://caml.inria.fr/pub/docs/old-311/libref/List.html#VALmap2).

### Signal Channels

This release also replaces the concept of an `Input` with `Signal.Channel`.
The API is extremely close to the ports API:

```haskell
channel : a -> Channel a
subscribe : Channel a -> Signal a
send : Channel a -> a -> Message
```

So now routing events in view code feels much more natural. If you are using
[elm-html][] your event handlers will look more like this:

[elm-html]: http://package.elm-lang.org/packages/evancz/elm-html/latest

```haskell
viewButton : Int -> Html
viewButton id =
    button
      [ onClick (send updateChan id) ]
      [ text (toString id) ]
```

It reads much more clearly now, hopefully making it easier to pick up. It also
has some nice conceptual connections to my [thesis][], so the door is open for
some cool stuff farther down the line!


## Making JSON Easier

Thanks to [Alexander Noriega](https://github.com/lambdatoast), we now have
great libraries for converting betwen JSON and Elm. The most crucial one is
[`Json.Decode`][decode] which gives you tools for converting JSON strings
to Elm. Here is a small example where we extract 2D coordinates from JSON.

```haskell
import Json.Decode (..)

type alias Point =
    { x : Float
    , y : Float
    }

point : Decoder Point
point =
  object2 Point
    ("x" := float)
    ("y" := float)

-- decodeString point "{ \\"x\\": 0, \\"y\\": 0 }" == Point 0 0
-- decodeString point "{ \\"x\\": 3, \\"y\\": 4 }" == Point 3 4
```

[decode]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Json-Decode

There are a ton more examples [here][decode]. It may be possible to do some
code generation when the JSON is very simple, but that is an idea for another
release!

Again, massive thank you to [Alexander Noriega](https://github.com/lambdatoast)
who had the key insight for this API. I cannot say enough times how happy I am
that JSON interop is solved!


## Making Randomness Easier

Thanks to [Joe Collard](https://github.com/jcollard/) working with randomness
is now much simpler and principled. The `Random` library provides the tools for
generating as many random values as you want, whenever you want. Furthermore,
it does it in a way that works great with time travel in [Elm Reactor][reactor].

[reactor]: /blog/Introducing-Elm-Reactor.elm

There is more info in [the `Random` docs](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Random),
but Joe also did a nice dice rolling example. You can check out the source code
[here](https://github.com/jcollard/random-examples/blob/master/src/TimeBasedDice.elm).

<iframe
    src="http://jcollard.github.io/dice-example/random-generator.html"
    style="display: block; margin: 0 auto;"
    width="300"
    height="320"
    frameborder="0"></iframe>


### Making Error Handling Easier

The `Either` library has been removed in favor of [the `Result` library]. A
`Result` is intended to be a very obvious choice for error handling. The core
type there is called a `Result`.

```haskell
type Result err value
    = Ok value
    | Err err
```

When you have a computation that may fail, like parsing or validating, you
want to return a `Result` that will either be `Ok` or an `Err` with some sort
of error message.

But why remove `Either` entirely you might ask? In my experience, the only
remaining times you might want an `Either` are when you might return two
different types or you need to differentiate between two different kinds of
values. Whenever I use an `Either` in my code, I end up regretting it later
when I cannot remember which thing was `Left` and which was `Right` or I end
up having to add another possibility, forcing me to refactor all of that code.
If you just start out being more specific by using a custom union type, both
of these problems go away, and I feel that is a better experience in practice.


## Better Build Tools

In addition to all changes in the core libraries, 0.14 also marks one of the
biggest refactors of the core tools around Elm. It introduces two new command
line tools:

  * [`elm-package`] &mdash; a package manager that resolves dependencies and
    enforces semantic versioning with API diffs (replacing `elm-get`)
  * [`elm-make`] &mdash; a build tool that knows how `elm-package` works and
    can do parallel builds (replacing `elm`)

You should read about the full details [here]() and [here]() before using 0.14.
One of the most interesting features of `elm-package` is how it enforces
semantic versioning. Rather than having programmers decide what is a major or
minor change, this tool does a diff on the public facing API. Depending on the
scope of those changes, it determines a new version number. This means package
authors will have to be extremely creative to break your code with a minor
version or patch version!

"""
