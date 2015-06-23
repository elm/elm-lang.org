import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


port title : String
port title = "Rethinking Type Errors in Elm"


main =
  Blog.blog
    "Rethinking Type Errors in Elm"
    "How small tweaks made a huge difference"
    Blog.evan
    (Blog.Date 2015 6 29)
    [ Center.markdown "600px" content
    , image "/assets/blog/error-messages/arg.png"
    , Center.markdown "600px" argComment
    , image "/assets/blog/error-messages/list.png"
    , Center.markdown "600px" listComment
    , image "/assets/blog/error-messages/if.png"
    , Center.markdown "600px" rest
    ]


image url =
  img [src url, style [("display", "block"), ("margin", "1em auto")]] []


content = """

When people start out with typed languages, they often feel antagonized by the
compiler.

> Check out this program I wrote!<br>`...NO`<br>Does it work if I change
this?<br>`...NO`<br>What if I mess with this thing?<br>`...NO`<br>Maybe you want
this?<br>`......YES`

Why is the compiler such an unhelpful jerk?!

Ease-of-use is a major priority in Elm, so I wanted to do better. I had heard
that improving error messages in type inferred languages was a very hard
*technical* problem, but as I worked on it, I found that **you can make massive
improvements by reframing it as a *user experience* problem.** It is truly
shocking how much better error messages get when you think about the fact that
they are going to be read by a human being.

So as of Elm 0.15.1 our error messages are dramatically improved. This means
Elm is easier than ever to learn and use!


## The New Messages

Lets look at a couple examples that illustrate some key aspects of the new
error messages.

First, lets try calling a function with a bad argument.

"""


argComment = """

**The error shows the code exactly as you wrote it.** It displays the line
number and underlines the root of the issue. Having a one-to-one mapping
between code and error messages means it is much easier to hop between these
two contexts, no mental translations are necessary! Turns out clang uses the
same technique to great effect.

Okay, now lets try to have a list with a mix of types that are not wrapped up
in a [tagged union](/guide/model-the-problem#tagged-unions).

"""


listComment = """

**Every message has a useful hint.** In this case, we can tell you exactly
which element of the list is causing issues. I found that generating such
specific error messages required no significant changes to the type inference
algorithm and imposed no noticable performance cost. We just add an extra bit
of info to each type constraint. I was shocked to find out that such huge
improvements could be made nearly for free.

Now lets see an example where an `if` leads us to two different types.

"""


rest = """

**Hints explain *why* something is an error.** In this case, the error explains
that both branches of an `if` need to result in the same type of value, guiding
you towards a fix.

These improvements are live in [the online editor](/try), so try playing with
[the examples](/examples) to see what it feels like when your error messages
are this nice!


## Help us improve!

We made a lot of progress with Elm 0.15.1 but we can definitely keep improving!
This is why we set up the [error-message-catalog][emc]. It is a collection of
Elm programs that trigger error messages, and it has already been a huge help
in finding problems and patterns and evaluating improvements.

[emc]: https://github.com/evancz/error-message-catalog

So if you run into an error message that is confusing, open an issue about it!
The [error-message-catalog][emc] is all about making your life better, and your
feedback makes that possible!


## Towards an IDE

Elm 0.15.1 introduces the `--report` flag to elm-make, allowing you to ask for
all your error messages and warnings to be spit out as JSON. The hope is that
an editor or IDE can use the compiler as a background service. I know elm-make
is quite fast, so perhaps it will be fast enough to support a proper IDE
experience. I’m not sure, but now we have the foundation to try it out and see!


## Stumbling Upon Success

Like I said at the beginning, the key was reframing a *technical* problem as a
*user experience* problem. As I write this up, I am realizing that we use this
technique all the time in Elm, whether its the time-traveling debugger or
automatic semantic versioning. When you reframe things around the user, you
can create experiences that seemed impossible before.

In this case, the key insights were made possible by airplanes. International
flights in particular create magical mental state where programming becomes way
easier. I imagine it is like temporarily becoming the programming version of a
[rap god](https://youtu.be/XbGs_qK2PQA). So whenever I go on a long flight, I
work on a big refactoring project. Something that would resolve some
longstanding issue, be acheivable without internet access, and generally be
fun.

On a recent trip to Beijing, I decided to spit out all error messages as JSON.
I wanted these messages to be available for editors or IDEs to do red underlines
on errors. That would be pretty cool! Thanks to China’s Great Firewall, it
turned out that my internet access was actually quite limited for a whole week.
Great news for me!

...


"""
