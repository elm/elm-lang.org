import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


main =
  Blog.blog
    "Compiler Errors for Humans"
    "Rethinking the terminal UX in Elm 0.15.1"
    Blog.evan
    (Blog.Date 2015 6 30)
    [ Center.markdown "600px" content
    , image "/assets/blog/error-messages/0.15.1/naming.png"
    , Center.markdown "600px" formattingComment
    , image "/assets/blog/error-messages/0.15.1/missing-field.png"
    , Center.markdown "600px" hintComment
    , image "/assets/blog/error-messages/0.15.1/list.png"
    , Center.markdown "600px" rest
    ]


image url =
  div [class "content", Center.style "900px"] [
    img
      [ src url
      , style [("display", "block"), ("margin", "1em auto")]
      , alt "compiler output example"
      ]
      []
  ]


content = """

Folks who prefer dynamically-typed languages are generally of the opinion that
**working with compiler error messages sucks**. Now before anyone goes into a
tired treatise about the virtues and benefits of types, think about the actual
concern here. A lot of compiler error messages actually *do* suck. Some of them
suck quite a lot. What happens when we accept that there is a problem here and
try to do better?

Ease of use is a major priority in Elm, so I recently took a couple weeks to
really focus on this. I learned that **you can make a shockingly huge
difference just by thinking about the user experience**. I am not ready to
claim that we totally solved things and have the best error messages ever, but
[many](https://twitter.com/rtfeldman/status/614552569327697921)
[folks](https://twitter.com/andrewdotnich/status/613237997014638592) are very
excited about our progress so far.

This post is going to go through a couple concrete user experience problems
that show up in every compiler I have used, showing how Elm 0.15.1 is starting
to improve the sorry state of compiler errors.


## Finding the Relevant Code

Before you can resolve an error, you need to find the code causing it. Seems pretty
obvious.

With many compilers you get a location like `program.x:43:22` that you have to
decipher. Where is that file? Which one is the line? Which is the column? Okay,
let me scan through my code. You also often get a pretty-printed version of the
problematic code, but it looks nothing like the code you wrote. You again need
to do a mental transformation to find it. So a lot of time is lost:

  * converting row and column numbers into an actual file position
  * converting pretty-printed code onto actual code to verify that position

The new Elm 0.15.1 error messages combat both problems directly.

"""


formattingComment = """

**The error shows the code exactly as you wrote it.** In this example, we have
some code to view users as HTML, but we misspelled `List.map`. The error
message shows the exact code you wrote *and* the corresponding line numbers.
Users can ask “does this look like that?” without really needing much conscious
analysis of lines and columns and code.

This alone makes a huge difference in how it *feels* to work with a compiler.
I have met a few folks who switched from gcc to clang mostly because of a
feature like this!


## Helpful Hints

Now that the error has been located, we need to fix it. But what is actually
going wrong?

With many compilers, you get a bunch of poorly formatted gobbledygook. It tells
you about what went wrong during *compilation*. Saying “these two types do not
match” is exactly went wrong for the compiler, but how the hell does that relate
to my code?! Again, you are doing a mental translation from “the compiler is
angry” to something actually useful. Why not try to do that automatically?

The Elm 0.15.1 messages try to minimize this translation time by providing
specific context and hints.

"""


hintComment = """

**Every message has a useful hint.** In this case, we learn that the first
argument to function `isOver50` is causing problems and it has to do with
missing a field named `age`. Pretty spot on!

This is actually a pretty common type of bug when you are doing refactors in
a decent size codebase. You maybe change the shape of a “person” but forget
about one or two functions that rely on the old definition.

This “bugs by refactor” risk exists in pretty much every language to some
extent. In [the equivalent JS code][js], you would just have to hope that tests
catch this. In fact, the JS version of `isOver50` thinks an ageless person is
under 50, a very sneaky bug! You would eventually find out about the missing
`age` field, but probably weeks later in a bug report. The risks introduced by
a refactor of thing in JS often means that people just refactor less, even if
it is “the right thing to do” by other metrics.

[js]: http://jsbin.com/xaheloboti/1/edit?js,console

Point is, having this extra line of defense in Elm is only truly nice if it
*feels* nice to use, and I think adding extra context makes a huge difference.
Whether you are using a compiler or interpreter, nobody wants a confusing and
rude gatekeeper.

> **Technical Note:** I found that generating such specific error messages
required no significant changes to the type inference algorithm and imposed no
noticeable performance cost. I just added an extra bit of info to each type
constraint. I was shocked to find out that such huge improvements could be made
nearly for free.


## Colors and Formatting

So far we have two pretty solid improvements, but we can do better by thinking
about the experience of actually reading the error message.

When we read code, **color is a huge usability improvement.** It provides some
redundant information that helps us recognize patterns more quickly. This can
definitely help us emphasize certain things and make skimming easier. I mean,
just check out how I am using bold phrases to draw your attention to certain
ideas!

When we read prose, **layout has a major impact on our experience.** Reading
10 pages of text with no paragraph breaks would just be hard. So whitespace
can help improve clarity. Furthermore, **context helps us understand what is
happening.** When reading a technical text, it is always easier if the author
has defined the terms they are using and the specific issue they are
addressing. It makes the details much more concrete.

Again, let's look at how the Elm 0.15.1 messages use these observations to
improve error messages. This example tries to make a list of user pictures, but
we mix up HTML and raw strings.

"""


rest = """

We use color in two ways here. The red draws your attention to the problem. It
is very easy to scan for that and filter out all other information. The blue
serves as a visual separator between error messages. I just want to look at one
message at a time, and these lines make sure I can easily find a single chunk.

We use layout to reveal detail as needed. General context is *above* the code.
It is very short and hints at what is wrong in the code snippet. Sometimes that
will be enough, which is great! If not, more specific hints are *below* the
code.


## Towards an IDE

So we have seen a bunch of ways to improve life in the terminal, but I began
this project with the goal of building compiler support for nice editor and IDE
features. We can do a lot on the terminal, but features like “jump to
definition” and red squiggles in your actual code are a whole ’nother level of
ease of use. Elm 0.15.1 makes some progress on that too! When you add the
`--report=json` flag, our build tool can now spit out JSON error messages that
are easy to read in to any editor plugin out there.

Joseph Hager has already done some amazing work bringing this information into
his [elm-vim][] plugin:

[elm-vim]: https://github.com/ajhager/elm-vim

<div class="intrinsic-container">
<iframe
  src="https://player.vimeo.com/video/132107269?color=49c180&title=0&byline=0&portrait=0"
  allowfullscreen></iframe>
</div>

(This is really exciting, great job!)

I have also done a bit in [the online editor](/try) as well to hint at what is
possible. So when you try out any of [the examples](/examples) you will see
hints in the top left linking you to documentation and error messages with a
“jump to error” button that makes finding the relevant code even easier!

I am very excited to see the community start getting these features working in
[all the Elm editor plugins](/install) that are out there! If this is something
you get excited about, there has never been a better time to start
collaborating on these tools. I hope we will see nice feedback loop between
editor features and compiler features!


## Final Thoughts

It is kind of shocking how much better things get when you focus on the user.
I mean, on some level, it is not shocking at all though. Most terminal tools
came into existence well before our industry really started focusing on making
apps and websites feel great for their users. We all collectively realized that
a hard to use app or website is bad for business, but the same lessons have not
really percolated down to tools like compilers and build tools yet. Hopefully
I have demonstrated that we can do better!

Speaking of doing better, I set up the [error-message-catalog][emc] to keep
improving Elm. It is a collection of Elm programs that trigger error messages.
It has already been a huge help in finding problems and patterns and evaluating
improvements. So if you run into an error message that is confusing, open an
issue about it. Your feedback will help us keep improving!

[emc]: https://github.com/evancz/error-message-catalog

Now go try out Elm’s new error messages in [the online editor](/examples) or
[on your machine](/install). See what it feels like!

"""
