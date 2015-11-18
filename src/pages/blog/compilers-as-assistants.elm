import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


port title : String
port title =
  "Compilers as Assistants"


main =
  Blog.blog
    "Compilers as Assistants"
    "Making Elm faster and friendlier in 0.16"
    Blog.evan
    (Blog.Date 2015 11 9)
    [ Center.markdown "600px" content
    , iframe
        [ width 560
        , height 315
        , src "https://www.youtube.com/embed/ARJ8cAGm6JE?start=60&end=88"
        , attribute "frameborder" "0"
        , attribute "allowfullscreen" ""
        , style
            [ "display" => "block"
            , "margin" => "1em auto"
            ]
        ]
        []
    , Center.markdown "600px" afterVideo
    , image "/assets/blog/error-messages/0.16/big-record.png"
    , Center.markdown "600px" afterTypeDiffs
    , image "/assets/blog/error-messages/0.16/context.png"
    , Center.markdown "600px" afterContext
    , image "/assets/blog/error-messages/0.16/expected-arg.png"
    , Center.markdown "600px" afterExpected
    , image "/assets/blog/error-messages/0.16/if-branches.png"
    , Center.markdown "600px" afterIf
    , image "/assets/blog/error-messages/0.16/string-hint.png"
    , Center.markdown "600px" afterStringAdd
    , image "/assets/blog/error-messages/0.16/truthy.png"
    , Center.markdown "600px" afterTruthy
    , image "/assets/blog/error-messages/0.16/incomplete.png"
    , Center.markdown "600px" afterIncomplete
    ]



(=>) = (,)


image url =
  img [src url, style [("display", "block"), ("margin", "1em auto")]] []


content = """

Compilers do not have the best reputation. Their intent is admirable: find
sneaky bugs, help fix them, and generate fast code. Sounds pretty wonderful!
The problem is that many compilers act like HAL 9000.

"""


afterVideo = """

I mean, no compiler is *literally* a malevolent AI that wants to kill you, but
sometimes it feels that way!

One of Elm’s goals is to change our relationship with compilers. **Compilers
should be assistants, not adversaries.** A compiler should not just *detect*
bugs, it should then help you understand *why* there is a bug. It should not
berate you in a robot voice, it should give you specific hints that help you
write better code. Ultimately, a compiler should make programming faster and
more fun!

Well, today marks the release of Elm 0.16 which I think is a great step in this
direction. This release:

   * produces dramatically better error messages
   * catches incomplete pattern matches
   * introduces tail-call optimization
   * removes redundant syntax to improve the “code texture” of Elm

Most importantly of these, **Elm compiler is now producing has the best error
messages I have ever worked with**. And I hope you will feel the same way!

Okay, enough overview, let’s dive into all the new stuff.


> **Note:** Maybe *you* have seen better error messages? If so,
[tell us](https://github.com/elm-lang/error-message-catalog/). Users reports of
confusing error messages actually motivated most of the improvements in this
release. We can only fix things if we know about them, so help us keep
improving!

> **Note:** If you just want to get started now, [get 0.16 installed](/install)
and read the [upgrade docs][upgrade] for a concise list of changes and a
thorough guide on upgrading your code. Should be quite simple and minimal!

[upgrade]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.16.md


# Improved Error Messages

In Elm 0.15.1 we took the first steps towards [compiler errors for humans][0.15.1]
and asked the community to provide feedback in the [error message catalog][emc].
Responding to this feedback has led to some extraordinarily nice error messages,
so I want to highlight some of the the user-reported issues that helped make this
happen!

[0.15.1]: /blog/compiler-errors-for-humans
[emc]: https://github.com/elm-lang/error-message-catalog


### Type Diffs

[Richard Feldman](https://github.com/rtfeldman) (who has been [using Elm at
NoRedInk][nre-blog] for about half a year now) opened [an issue][big-record]
describing a problem with big records. [The error message][big-record-comment]
showed types so large that it was hard to pinpoint the root problem. He
suggested a “type diff” to filter out the useless and distracting parts.
This was a great idea, so I did it! The new message looks like this:

[nre-blog]: http://noredinktech.tumblr.com/post/126978281075/walkthrough-introducing-elm-to-a-js-web-app
[big-record]: https://github.com/elm-lang/error-message-catalog/issues/23
[big-record-comment]: https://github.com/elm-lang/error-message-catalog/issues/23#issue-95370810

"""


afterTypeDiffs = """

The Elm compiler now does **type diffs** where it compares any two types and
highlights the differences. Notice that it hides information that is not directly
relevant, so Richard just sees the fields that do not match. It even does typo
detection for record fields, so there is a nice little hint pointing out the
typo that caused the problem!


### More Context

In previous releases, the compiler would only show you the part of the types
that clashed. If we always showed the *entire* type, Richard’s problem with big
records would have been even worse!

Zach Gotsch reported [a nice minimal example][context] of how this lack of
context leads to confusing error messages. Well, thanks to type diffs it is not
a big deal to show the entire problematic type and just highlight the part that
is causing an issue!

[context]: https://github.com/elm-lang/error-message-catalog/issues/53

"""


afterContext = """

### Expected vs Actual

One of the most common questions about type errors has been “Why doesn’t it
tell me which type was expected and which was actually given?” As you may have
noticed in the previous examples, that is in this release as well!

In the most basic example, you just get some information about what the
function expects:

"""


afterExpected = """

As I implemented this, I realized that this expected vs actual dichotomy does
not *really* make sense in a lot of cases. A simple example of this is when the
branches of an `if` do not match. Which is the expected one? Are they both actual?
It just does not really make sense, so instead of settling for some convention,
the compiler just “does the right thing” for each scenario. For example, when
the branches of an `if` do not match it looks like this:

"""


afterIf = """

I found it quite surprising and delightful that “expected vs actual” does not
make sense in many cases, so I wrote a bit more about this
[here](https://groups.google.com/forum/#!topic/elm-dev/878AcvEQ5tk).


### Beginner Hints

When you are starting out with a new language, there are a few things that you
are pretty much guaranteed to run in to. [Jessica Kerr](https://twitter.com/jessitron)
is an excellent developer and speaker who recently got started with Elm, and
she kindly reported all the times she ran into something weird. For example,
when appending strings in Elm you use the `(++)` operator. Thanks to [Jessica’s
suggestion][plus], Elm 0.16 gives a helpful hint when it sees folks trying to
add strings.

[plus]: https://github.com/elm-lang/error-message-catalog/issues/38

"""


afterStringAdd = """

Once I figured out how to cover this case, I added a couple other hints in the
same spirit. For example, you get a helpful hint when you try to do something
“truthy” in Elm.

"""


afterTruthy = """

There are quite a few others in there. Maybe you will see them when you
[install Elm](/install) and try it out ;)


### No More Cascading Errors

You see “cascading errors” when one thing goes wrong, and then this initial
wrongness leads to a bunch of other things going wrong. So with many compilers
a single mistake can lead to 3 or 4 different error messages, leaving the
programmer to figure out which one is the *real* problem.

Well, there are no more cascading errors in Elm 0.16 thanks to Hacker News!
When we announced our first effort to improve Elm’s error messages, someone on
Hacker News commented with a very simple yet specific description of [how to
avoid cascading errors][hn]. I had been trying to figure this out for a while
without success, and it turns out this suggestion works great: simple
implementation, no performance penalty, and no more cascading errors. Thanks
internet person!

[hn]: https://news.ycombinator.com/item?id=9808317


# Catching More Bugs

In addition to showing *better* error messages, this release is also catching
*more* errors. One big goal of Elm is to have no runtime errors. In practice,
this is pretty much already how it goes. You can go months or years without a
runtime error. In any case, Elm 0.16 closes one of the last remaining loopholes.

Thanks to [Izzy Meckler](https://github.com/imeckler), the Elm compiler
now detects “incomplete pattern matches” which are when a `case` expression
does not handle all possible cases. For example, say we are pattern matching
on a list to get the last element:

```elm
last : List a -> a
last list =
  case list of
    x :: [] ->
      x

    x :: rest ->
      last rest
```

If someone were to give an empty list to `last` it just would not know what to
do. That scenario is not covered. So this release detects these cases and shows
a helpful error message:

"""


afterIncomplete = """

This is particularly helpful when you have a large codebase and add a tag to a
union type. Now the compiler will point out all the `case` expressions scattered
throughout your code that need to have an extra branch added to them!


# Generating Faster Code

This summer we had two interns working on Elm, whose work will be coming out
over the next few months. [Joey Eremondi](https://github.com/JoeyEremondi)
focused on the compiler and performance, setting up some benchmarks for
generated code and catalyzing a decent number of optimizations.

So this release also introduces an improved code generation pipeline. With some
relatively minor changes, the number of closures in the generated JavaScript is
significantly decreased. This alone led to some [quite dramatic performance
improvements][perf].

[perf]: https://gist.github.com/evancz/76b619a83a6650a89918

Not only is code faster in general, this release also implements tail-call
elimination for self-recursive functions. This means that for some subset of
recursive functions, we can convert it to a while-loop under the hood. This
is a ton faster. Hopefully browsers start implementing “proper tail-calls” as
is demanded by ES6 and we will see this kind of stuff happening in more
scenarios.

Joey, thank you for your work on these improvements! It is always nice when a
new version of software comes out and things actually go *faster*.


# Removing Syntax

This release is also removing some obscure or confusing syntax from Elm. The
“personality” of Elm tends towards having a very small toolbox that covers a
shocking range of scenarios. So these removals are geared towards stuff that
I thought might be a good idea, but after seeing them in practice over the years,
we found that they subtly guided you away from great code.

You can see the full list of changes [here][upgrade-docs]. Many are focused on
slimming down the language so that tools like `elm-format` work even better,
but the biggest change is to record updates. Instead of using the backward
arrow, it will just be an equals sign now. We had gotten [feedback along these
lines][equals] for quite some time, especially from folks just starting out.

[upgrade-docs]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.16.md
[equals]: https://github.com/elm-lang/error-message-catalog/issues/16


# Thank You

It has been a pretty busy couple months for Elm so there are a lot of folks to
thank.

Thank you again to [Izzy](https://github.com/imeckler) and
[Joey](https://github.com/JoeyEremondi) who contributed larger projects
directly to this release. Thank you to everyone who tried out the alpha
releases and reported issues. I recall some good ones caught by
[Max](https://github.com/mgold) and [Aaron](https://github.com/avh4). Thank
you to [Janis](https://github.com/jvoigtlaender) for curating and resolving a
bunch of issues on core repositories! Whether it is `core` or `package.elm-lang.org`
you always have great suggestions and end up getting “the right thing”
implemented even when I am a stickler at first.

I also want to thank the Elm community. We have been going through some
growing pains recently with lots of new folks showing up, and a lot of
community members have stepped up to keep things running smoothly. Thank you
in particular to Richard, Pete, Joey, Jeff, Max, and Janis. This process
has been a bit rough on me, and I do not know how to appropriately thank all
the people who have been supportive or just said “I bet I can do this better”
and went for it!

<br>

"""