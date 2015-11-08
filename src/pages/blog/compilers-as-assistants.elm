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
    , image "/assets/blog/error-messages/0.16/big-record.png"
    , Center.markdown "600px" afterTypeDiffs
    , image "/assets/blog/error-messages/0.16/context.png"
    , Center.markdown "600px" afterContext
    , image "/assets/blog/error-messages/0.16/string-hint.png"
    , Center.markdown "600px" afterStringAdd
    , image "/assets/blog/error-messages/0.16/truthy.png"
    , Center.markdown "600px" afterTruthy
    ]
{-
    , iframe
        [ width 560
        , height 315
        , src "https://www.youtube.com/embed/ARJ8cAGm6JE"
        , attribute "frameborder" "0"
        , attribute "allowfullscreen" ""
        , style
            [ "display" => "block"
            , "margin" => "1em auto"
            ]
        ]
        []
    , Center.markdown "600px" afterVideo
    ]
--}

(=>) = (,)


image url =
  img [src url, style [("display", "block"), ("margin", "1em auto")]] []


content = """

<span style="color: red;">DRAFT - DO NOT SHARE</span>

Compilers do not have the best reputation. Their intent is admirable: find
sneaky bugs, help fix them, and generate fast code. Sounds pretty wonderful!
The problem is that **many compilers act like HAL 9000**. Programming is
definitely less fun when your assistant is [a malevolent AI that wants to kill
you][hal].

[hal]: https://youtu.be/ARJ8cAGm6JE

It does not have to be this way! Today marks the release of Elm 0.16 which
has the best error messages I have ever worked with.

> QUOTE.

In addition to [dramatically improved error messages](#improved-error-messages),
this release also:

  * [catches more bugs](#catching-more-bugs), including incomplete pattern matches
  * [generates faster code](#generating-faster-code) and introduces tail-call elimination
  * [removes some confusing and redundant syntax](#removing-syntax)

If you just want to get started now, [get 0.16 installed](/install) and read
the [upgrade docs][upgrade] for a concise list of changes and a thorough guide
on upgrading your code. Should be quite simple and minimal!

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

[Richard Feldman](https://github.com/rtfeldman) is the lead frontend developer
at NoRedInk. They have been [using Elm in production][nre-blog] for about half
a year now and probably have the biggest commercial Elm code base. He opened
[an issue][big-record] describing the error message he got when a big record
was not quite right.

[nre-blog]: http://noredinktech.tumblr.com/post/126978281075/walkthrough-introducing-elm-to-a-js-web-app
[big-record]: https://github.com/elm-lang/error-message-catalog/issues/23

[The error message][big-record-comment] was very long and did not give a very
clear indication of what actually went wrong. Richard suggested a “type diff”
that would make it easy to immediately see the problem. This was a great idea,
so I did it! The new message looks like this:

[big-record-comment]: https://github.com/elm-lang/error-message-catalog/issues/23#issue-95370810

"""


afterTypeDiffs = """

The Elm compiler now does **type diffs** where it compares any two types and
highlights the differences. Notice that it elides information that is not directly
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

### Beginner Hints

When you are starting out with a new language, there are a few things that you
are pretty much guaranteed to run in to. [Jessica Kerr](https://twitter.com/jessitron)
is an excellent developer and speaker who recently got started with Elm. As she
got familiar with Elm, she kindly reported all the times she ran into something
weird. For example, when appending strings in Elm you use the `(++)` operator.
Thanks to [Jessica’s suggestion][plus], Elm 0.16 gives a helpful hint when it
sees folks trying to add strings.

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
avoid cascading errors][hn]. I had been trying to figure out for a while
without success, and it turns out this suggestion works great: simple
implementation, no performance penalty, and no more cascading errors. Thanks
internet person!

[hn]: https://news.ycombinator.com/item?id=9808317


# Removing Syntax

https://github.com/elm-lang/error-message-catalog/issues/16


# Catching More Bugs


# Generating Faster Code
"""