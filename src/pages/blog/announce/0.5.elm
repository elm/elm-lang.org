import Blog
import Center


main =
  Blog.blog
    "Elm 0.5"
    "The Libraries you Need"
    Blog.evan
    (Blog.Date 2012 10 25)
    [ Center.markdown "600px" content ]


content = """

This release focuses on growing [Elm](/)'s standard libraries to make sure you always have the tools you need.
For a full listing of Elm's current libraries, see [this page][docs].

  [docs]: /docs "docs"

### Dictionaries and Sets

Elm now has [dictionaries][Dict] and [sets][Set]!

  [Dict]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Dict "Dictionary library"
  [Set]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Set "Set library"

The Dict and Set libraries could be used from JavaScript. I can make this easier if people are interested. Let me know!

### Automatons

This version also introduces the [Automaton][auto] library. This library will
make it easier to create dynamic components that can be switched in and out of a program.

  [auto]: http://package.elm-lang.org/packages/evancz/automaton/1.0.0/ "Automaton Library"

&ldquo;But what is an automaton?&rdquo; you might be asking. An automaton is like a little robot that
takes inputs and produces outputs. Without input, an automaton just sits quietly, waiting for something to do.
Automatons can have a &ldquo;memory&rdquo; so their output may depend on their past experiences. All automatons
are interchangeable, so they are easy to switch in and out of programs.

This library is based on the very clever ideas introduced by [Arrowized FRP][afrp].
I have made an effort to make it easier to understand and use for people unfamiliar with
&ldquo;Arrows&rdquo; and other concepts that are largely orthogonal to doing-things-in-real-life.
I am hoping that the term [&ldquo;automaton&rdquo;][wiki] is somewhat familiar (or at least
a better anology than &ldquo;arrow&rdquo;). Huge thanks to Joey Adams for suggesting this library
and working through the details with me!

I plan on writing some blog posts on automatons, so hopefully that will make it clearer why they
are totally rad.

  [wiki]: http://en.wikipedia.org/wiki/Automata_theory#Informal_description "automaton wiki"
  [afrp]: http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf "Arrowized FRP"

### Escape from &ldquo;Callback Hell&rdquo; (new HTTP library)

You can now make pretty much any kind of request you want ([docs][http]). You can specify verb, url, payload, and custom headers.

This library makes it possible to escape &ldquo;callback hell&rdquo;. The [`send`][send] function takes in a signal of
HTTP requests and produces a signal of responses. The responses only update when they are ready. You can use
it just like any other signal, but it updates asynchronously, so you can write nice code that is both readable
and efficient. No callbacks! No nested-callbacks! ...

See this library in action with the [Zip Code fetcher][zips].

I will be writing more about this library fairly soon
because I think it is an important and novel part of Elm.
JS developers struggle with &ldquo;callback hell&rdquo; on a daily basis, and
now they do not have to!

  [send]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Http#send "send"
  [http]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Http "HTTP docs"
  [zips]: /examples/zip-codes

### New Functions and Syntax

- Abbreviated notation for tuple functions:
    * `(,)  === (\\x y -> (x,y))`
    * `(,,) === (\\x y z -> (x,y,z))`
    * etc.
- New functions for converting strings to numbers. Great for text input boxes:
    * `readInt : String -> Maybe Int`
    * `readFloat : String -> Maybe Float`
- [`(complement : Color -> Color)`][color] which computes complementary colors! Surprisingly difficult to do!

  [color]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Color "Color library"

### Fewer Library Prefixes

The library prefixes have pretty much all been removed. `Data.List` is now `List`, `Signal.Mouse` is now `Mouse`, etc.
The prefixes end up being more confusing than helpful. `Data.List` makes it sound like some special version for no reason.

This is a breaking change, but one that I think makes Elm a nicer language.
The fix is just a matter of taking some words out of your `import` statements,
but please email [the list][list] if you want assistance with this (e.g. a script or some advice).

### Bug fixes, Optimizations, and Error Messages

- Generated JS is more readable.
- Pattern matching is smaller and faster in generated JS.
- Fix a bug with pattern parsing. `Four _ _ B _` was parsed as `Four _ _ (B _)`.
- `String` is now treated as an alias of `[Char]`.
- Better type error reporting for ambiguous uses of variables and for variables in aliased modules.

## Other News

I recently started an [`#elm` IRC channel at freenode][irc], so feel free to come hang out
and chat. Big thank you to `tac` for helping get the channel set up!

In other cool news, Elm just got its 100th star on [github][github]! Yay growth!

If you want to help out, there are [tons of ways to contribute][contribute]!

  [contribute]: https://github.com/elm-lang/elm-compiler/blob/master/CONTRIBUTING.md "how to contribute"
  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "Elm-Discuss"
  [github]: https://github.com/evancz "Elm on GitHub"
  [irc]: http://webchat.freenode.net/ "IRC"

"""

