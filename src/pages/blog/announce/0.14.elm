import Blog
import Center


main =
  Blog.blog
    "Elm 0.14"
    "Simpler Core, Better Tools"
    Blog.evan
    (Blog.Date 2014 12 10)
    [ Center.markdown "600px" content ]


content = """

This release has two major aspects, both focusing on making it easy and quick
to start making beautiful projects with Elm:

  * **Simplify the language and core libraries.**<br>
    Signals are easier. JSON and random number generation are both massively
    improved. Error handling is clearer. Markdown parsing now lives in [a
    library][elm-markdown], making it much more flexible. Types easier to learn
    and understand. The net effect of these improvements ripple out to all
    aspects of Elm, making relatively untouched things like [elm-html][] feel
    like they got upgraded too.

[elm-html]: http://package.elm-lang.org/packages/evancz/elm-html/1.1.0
[elm-markdown]: http://package.elm-lang.org/packages/evancz/elm-markdown/1.0.0

  * **New package manager and build tool.**<br>
    The new package manager, [`elm-package`][elm-package], has a fresh take on
    alleviating dependency  hell. It reliably detects API changes, so we can
    create a nice human-readable list of additions, changes, and removals
    between any two versions. For users, this means it is much easier to figure
    out if you want to make an upgrade. Now there is a nice list of what you
    will need to change. For package authors, it means we can automatically
    enforce semantic versioning. No user will ever get a breaking API change in
    a patch version again! More automation and verification is planned, and we
    now have a solid foundation to build upon. This release also introduces
    [`elm-make`][elm-make] which
    is a build tool that replaces the old `elm` command. It permits parallel
    compilation and can handle any package downloaded with `elm-package`.

[elm-package]: https://github.com/elm-lang/elm-package#elm-package
[elm-make]: https://github.com/elm-lang/elm-make#elm-make

I am really excited about this release. If you have been eyeing Elm from afar,
now is a great time to start taking a closer look. It feels like everything
is coming together. My goal has always been to make web programming pleasant,
but I never thought it would be quite this nice.

If you are in a rush or just want references to help you upgrade, follow the
[install instructions](/install) and then check out
[the changelog](https://github.com/elm-lang/core/blob/master/changelog.md#014),
[elm-package][], and [elm-make][].

This post dives into the most exciting changes, showing how they connect to the
broader philosophy that guides all of the improvements in 0.14. I hope this will
give people some idea of what Elm is about and where it is going.

## Guiding Philosophy

A great teacher takes an idea and makes you *feel* it. They make it exciting
and alive. All the bad explanations you have heard before melt away, and you
feel the rush of comprehension.

I think [Elm Reactor][reactor] embodies this. I think the [online editor](/try)
and [examples](/examples) embody this. I want to design Elm such that this
is happening in syntax and libraries. As much as possible, I want people to see
some code for the first time and *feel* how it works. I want to bring the
learning curve down from days to minutes.

[reactor]: /blog/time-travel-made-easy


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

This is not some pet theory I formed in a vacuum. Between teaching functional
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

```elm
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

```elm
type UserID = OldID Int | NewID String

toNewID : UserID -> String
toNewID userID =
    case userID of
      OldID number -> toString number
      NewID string -> string
```

You would probably not believe how many times I found strangers and presented
this idea a bunch of different ways. I was actually somewhat surprised to end
up with union type, but it just worked really well.

One benefit is that someone can read the term &ldquo;union type&rdquo; randomly
on some forum or hear it in a conversation and have a pretty good idea what it
is. If you want to be extra precise, the term [tagged union][tagged] can be
helpful. Comparing union types to Java-style enumerations can also be successful
depending on who you are talking to.

Even with the best terminology, it can still be tough to give a good
explanation. For people who want to *teach* this concept, I have written up
[a document][gist] that attempts to handle common questions gracefully. I have
also written up [a full description][union] that explains what they are and
shows a bunch of examples.

[union]: /guide/model-the-problem
[gist]: https://gist.github.com/evancz/06fe634245a3aab4a61b
[tagged]: http://en.wikipedia.org/wiki/Tagged_union


### List Types

The special syntax for list types has been removed. Working with lists now
looks more like this:

```elm
four : Int
four = length [1,2,3,4]

length : List a -> Int
length list =
  case list of
    [] -> 0
    first :: rest ->
        1 + length rest
```

Notice that the type uses `List a` instead of `[a]`. The primary benefit here
is consistency across all type signatures. In addition to simplifying the
learning process, this also makes it easier to switch type annotations between
`List`, `Set`, and `Array` depending on what you want to do. Finally, it frees
up the `[]` syntax in types just in case that could come in handy some day.


## Making Signals Easier

### No More Lifting

The term `lift` is dead. It makes me a bit sad for my [thesis][], but I think
it will help a lot of people get started with signals more quickly. The new
term is `map`, and the goal is to build on the intuition people have from
working with lists.

[thesis]: /assets/papers/concurrent-frp.pdf

```elm
Signal.map  : (a -> b) -> Signal a -> Signal b
Signal.map2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
```

To make this connection stronger, the `List` library has changed a bit too.
Instead of having a bunch of `zip` and `zipWith` functions, everything has
become a variation of `map`. When you want to put many lists together,
combining values pairwise, you use the `map2` function.

```elm
List.map  : (a -> b) -> List a -> List b
List.map2 : (a -> b -> c) -> List a -> List b -> List c
```

So if you want to put two lists together, you write expressions like this:

```elm
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

```elm
channel : a -> Channel a
subscribe : Channel a -> Signal a
send : Channel a -> a -> Message
```

So now routing events in view code feels much more natural. If you are using
[elm-html][] your event handlers will look more like this:

[elm-html]: http://package.elm-lang.org/packages/evancz/elm-html/1.1.0

```elm
viewButton : Int -> Html
viewButton id =
    button
      [ onClick (send updateChan id) ]
      [ text (toString id) ]
```

It reads much more clearly now, hopefully making it easier to pick up. It also
has some nice conceptual connections to my [thesis][], so the door is open for
some cool stuff farther down the line. Huge thanks to [Richard
Feldman](https://github.com/rtfeldman/) for talking through this
until we ended up with this API! Your insights from writing
[dreamwriter](https://github.com/rtfeldman/dreamwriter#dreamwriter) in Elm
have been extremely valuable in general.


## Making JSON Easier

Thanks to [Alexander Noriega](https://github.com/lambdatoast), we now have
great libraries for converting between JSON and Elm. The most crucial one is
[`Json.Decode`][decode] which gives you tools for converting JSON strings
to Elm. Here is a small example where we extract 2D coordinates from JSON.

```elm
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
who had the key insight for this API. I have been seeing huge bumps in code
quality and readability as I translate to the new API.


## Making Random Number Generation Easier

Thanks to [Joe Collard](https://github.com/jcollard/), working with random values
is now much simpler and principled. [The `Random` library][random] provides the
tools for generating as many random values as you want, whenever you want.
Furthermore, it does it in a way that works great with time travel in
[Elm Reactor][reactor].

[random]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Random
[reactor]: /blog/time-travel-made-easy

There is more info in [the `Random`
docs](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Random),
but Joe also did a nice dice rolling example. You can check out the source code
[here](https://github.com/jcollard/random-examples/blob/master/src/Dice.elm).

<iframe
    src="http://jcollard.github.io/dice-example/"
    style="display: block; margin: 0 auto;"
    width="300"
    height="320"
    frameborder="0"></iframe>


## Making Error Handling Easier

The `Either` library has been removed in favor of [the `Result` library][result].
A `Result` is intended to be a very obvious choice for error handling. The core
type there is called a `Result`.

[result]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Result

```elm
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

In addition to all the changes in the core libraries, 0.14 also marks one of
the biggest refactors of the core tools around Elm. It introduces two new
command line tools:

  * [`elm-package`][elm-package] &mdash; a package manager that resolves dependencies and
    enforces semantic versioning with API diffs (replacing `elm-get`)
  * [`elm-make`][elm-make] &mdash; a build tool that knows how `elm-package` works and
    can do parallel builds (replacing `elm`)

You should read about the full details [here][elm-package] and [here][elm-make]
before using 0.14. One of the most interesting features of `elm-package` is API
diffing. For example, let&rsquo;s say I am curious what changed between
versions 1.0.0 and 1.1.0 of the new [elm-markdown][] library. I would run the
following command:

```
elm-package diff evancz/elm-markdown 1.0.0 1.1.0
```

Resulting in a print out of all the changes.

```
Comparing evancz/elm-markdown 1.0.0 to 1.1.0...
This is a MINOR change.

------ Changes to module Markdown - MINOR ------

    Added:
        type alias Options =
            { githubFlavored : Maybe { tables : Bool,
                                       breaks : Bool
                                     },
              sanitize : Bool,
              smartypants : Bool
            }
        defaultOptions : Options
        toElementWith : Options -> String -> Element
        toHtmlWith : Options -> String -> Html
```

I see that you can now tweak the settings of the markdown parser, which will
have no impact on my existing code. Totally safe to upgrade! Longer term, I
would like to estimate &ldquo;upgrade costs&rdquo; by finding how many times
changed or removed values appear in your existing code.

The benefits are actually much deeper though. Now that we know exactly how
the API has changed, it is possible to automatically enforce [strict
versioning rules](https://github.com/elm-lang/elm-package#version-rules). If
there are breaking changes, the new release *must* be a major version bump. As
a package user this is great because you have a guarantee that minor and
patch changes will not introduce breaking API changes. We have a lot more
planned that builds on top of this foundation. I think we are headed in a
good direction, so I will write more about the vision for `elm-package` in a
future blog post.

Big thanks to [Andrew Shulayev](https://github.com/ddrone) who did the first
iteration of the most important `elm-package` features during his summer
internship. And huge thanks to Prezi for making this internship possible!


## Thank you

Thank you to everyone who helped make 0.14 possible, whether that was with code
contributions, cool ideas, feedback on the release itself, participating in
thoughtful discussions on the mailing list, or making cool stuff totally
independently. I feel very lucky to work with such great people!

"""
