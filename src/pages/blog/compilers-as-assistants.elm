import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


main =
  Blog.blog
    "Compilers as Assistants"
    "Making Elm faster and friendlier in 0.16"
    Blog.evan
    (Blog.Date 2015 11 19)
    [ Center.markdown "600px" content
    , div [Center.style "600px"]
        [div [ class "intrinsic-container" ]
          [ iframe
              [ src "https://www.youtube.com/embed/ARJ8cAGm6JE?start=60&end=87&rel=0&autoplay=0"
              , attribute "allowfullscreen" ""
              ] []
          ]
        ]  
    , Center.markdown "600px" afterVideo
    , image "big-record"
    , Center.markdown "600px" afterTypeDiffs
    , image "context"
    , Center.markdown "600px" afterContext
    , image "expected-arg"
    , Center.markdown "600px" afterExpected
    , image "if-branches"
    , Center.markdown "600px" afterIf
    , image "string-hint"
    , Center.markdown "600px" afterStringAdd
    , image "truthy"
    , Center.markdown "600px" afterTruthy
    , image "incomplete"
    , Center.markdown "600px" afterIncomplete
    ]



(=>) = (,)


image name =
  div [class "content", Center.style "600px"] [
    img
      [ src ("/assets/blog/error-messages/0.16/" ++ name ++ ".png")
      , style [("display", "block"), ("margin", "1em auto")]
      , alt "compiler output example"
      ]
      []
    ]

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

Most importantly of these, **Elm compiler is now producing the best error
messages I have ever worked with**. And I hope you will feel the same way!

Okay, enough overview, let’s dive into all the new stuff.


> **Note:** Maybe *you* have seen better error messages? If so,
[tell us](https://github.com/elm-lang/error-message-catalog/). Users’ reports
of confusing error messages actually motivated most of the improvements in this
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
so I want to highlight some of the user-reported issues that helped make this
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
is causing an issue:

[context]: https://github.com/elm-lang/error-message-catalog/issues/53

"""


afterContext = """

### Expected vs Actual

The most common question about type errors in Elm has long been “Why doesn’t
it tell me which type was expected and which was actually given?” As you may
have noticed in the previous examples, Elm 0.16 adds this information!

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


# Tail Call Optimization

[Joey Eremondi](https://github.com/JoeyEremondi) did a bunch of work this
summer on compiler optimizations, so Elm 0.16 makes things quite a bit faster
as well!

Elm now does tail-call optimizations for self-recursive functions. Lots of
fancy words here, but the real meaning is pretty simple. Certain recursive
functions can be turned into `while` loops now. This is a ton faster and does
not grow the stack.

Turns out that ES6 will be getting a more advanced version of this feature
([details][es6-tail-call]), but it is not clear how long it will be before this
starts appearing in popular browsers. In any case, we get some of this now and
it happens at compile time.

[es6-tail-call]: http://benignbemine.github.io/2015/07/19/es6-tail-calls/

To get all this working, we ended up improving the whole code generation
pipeline. With some relatively minor changes, the number of closures in the
generated JavaScript is significantly decreased. This alone led to some [quite
dramatic performance improvements][perf].

[perf]: https://gist.github.com/evancz/76b619a83a6650a89918

Joey, thank you for your work on these improvements!


# Removing Syntax

Normally languages keep getting bigger, but Elm 0.16 is actually smaller and
more focused. All the changes are listed [here][upgrade-docs], so we will focus
on the two major changes here: removing multi-way `if` and removing
field addition/deletion.

[upgrade-docs]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.16.md#updating-syntax


### Simplified Ifs

Ever since [Laszlo][] realized [how to make Elm code look great][style], we
have been trying to get all Elm developers to use a “professional” style.

[Laszlo]: https://github.com/laszlopandy
[style]: /docs/style-guide

Very very early on (maybe in 0.1?) I added some syntax called a multi-way if.
  It looked like this:

```elm
    if | n < 0 -> -1
       | n > 0 -> 1
       | otherwise -> 0
```

It is kind of cool, but it has three major weaknesses that I did not appreciate
at the time.

**First**, it works against nice indentation in a lot of ways. According to
[the style guide][style], you should pick 2 or 4 space indent and stick with
that for your whole file. The vertical bar in the multi-way if kind of *wants*
3 space indent. It just looks really bad without it. The style guide also says
you should *always* bring values down a line after `=` or `->` so changing
something simple never causes a big stylistic refactor as well. Again,
multi-way ifs kind of call to you to not follow this rule because it would lead
to 8 space indents or even uglier things.

**Second**, it is possible to have “incomplete” multi-way ifs. It lets you write
silly stuff that can crash:

```elm
    if | n < 0 -> -1
       | n > 0 -> 1
```

If `n` is zero there is no branch to take, so this would just crash.
Since 0.16 is actually catching all incomplete pattern matches, it felt
particularly egregious to allow this kind of thing.

**Third**, it is totally redundant. You can do the same things with the normal
if/then/else syntax. So any multi-way if will turn into something like this:

```elm
    if n < 0 then
        -1

    else if n > 0 then
        1

    else
        0
```

This version *wants* to be 2 or 4 space indented. It *wants* to have the body
of each branch brought down a line. The aesthetics draw you towards writing
higher quality code. It also gets rid of the problem of “incomplete” ifs.

There has been some work on `elm-format` going on, so this also gets the
language in better shape for that.

> **Note:** Notice that the new version looks a whole lot more like Python or
Ruby or JavaScript or C++ or all the other languages. I think many popular
languages have a “code texture” that works really well for skimming and
recognizing structure. I think having great code texture is mostly a matter of
personal style, not the language or paradigm, so we will keep working to
discover the most excellent code texture!

> **Note:** We did [a ton of brainstorming][if-issue] to think through all the
possible variations here. It turns out the simple solution also works the best!

[if-issue]: https://github.com/elm-lang/elm-plans/issues/3



### Simplified Records

There is kind of a long story here. The short version is that record update
uses a normal equals sign now, like this:

```elm
    { point | x = 4 }
```

Instead of using the backwards arrow `<-` like before. This was something that
a lot of people got tripped up on, [even after they had a lot of
experience][equals], so overall I think this will make things a bit friendlier.
The backwards arrows also led to weird code texture when used within a `case`
such that you have forwards and backwards arrows going everywhere.

[equals]: https://github.com/elm-lang/error-message-catalog/issues/16

Okay, but the long story is interesting if you are into language design!

Elm uses a very cool record system. It is based on [an excellent paper][daan]
by Daan Leijen that lets you add and remove fields from records, all while
keeping the types simple. I really love this mix of power and simplicity!

[daan]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf

I added support for this back in 0.7, and at the time, I had never seen a
language (with a real working compiler) that allowed field addition and
deletion like this. So I had intuition, but no way to get real experience. I
worried that it *could* encourage overly complex code, so from the start I
was very conservative, knowing that we could expand or contract the features
as we got more data.

Well it has been more than two years since then, and the results are in. Pretty
much no one ever used field addition or deletion. In the few cases where people
*did* use it, it got pretty crazy pretty quickly. The one real-world case I
know of is recorded [here][extension-example] if you want to see, and the code
could be rewritten with union types, which turned out nicer anyway.

[extension-example]: https://github.com/elm-lang/elm-compiler/issues/985#issuecomment-121927230

Removing addition and deletion will also make Elm easier to optimize (described
more [here][extension-issue]). This is especially true if we are targeting
platforms besides JavaScript, but allows some simplifications in JS too. In
fact, [the benchmarks we ran for 0.16][perf] show that this made record updates
a lot faster!

[extension-issue]: https://github.com/elm-lang/elm-compiler/issues/985


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
