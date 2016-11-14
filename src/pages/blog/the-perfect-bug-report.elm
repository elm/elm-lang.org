import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


main =
  Blog.blog
    "The Perfect Bug Report"
    "Debugging with Elm 0.18"
    Blog.evan
    (Blog.Date 2016 11 14)
    [ Center.markdown "600px" intro
    , debuggerDemo
    , Center.markdown "600px" content
    , badHistory
    , Center.markdown "600px" afterError
    ]



-- HELPERS


debuggerDemo : Html msg
debuggerDemo =
  div [ Center.style "800px" ]
    [ div [ class "intrinsic-container" ]
        [ iframe
            [ src "https://www.youtube.com/embed/oNogm31F2mo?rel=0&autoplay=0&showinfo=0&color=white&modestbranding=1"
            , attribute "allowfullscreen" ""
            ]
            []
        ]
    ]


badHistory : Html msg
badHistory =
  div [class "content", Center.style "830px"] [
    img
      [ src "/assets/blog/0.18/error.png"
      , style [("display", "block"), ("margin", "0 auto")]
      , alt "attempting to load incompatible history"
      ]
      []
  ]



-- CONTENT


intro = """

**Reproducing bugs is awful.** You get an issue like “Problem with Sidebar” that vaguely describes some odd behavior. Now you must *somehow* reproduce it exactly. Was it the specific timing of events? Was it bad data from the server? Was it specific to a certain user? Was it a recently updated dependency? As you slog through all these possibilities, the most annoying thing is that the person who opened the bug report already had all this information! **In an ideal world, you could just replay their exact session.**

**Elm 0.18 lets you do exactly that!** In debug mode, Elm lets you import and export the exact sequence of events from a program. You get all the information necessary to reproduce the session exactly, from mouse clicks to HTTP requests. [**Try it yourself!**][demo] Or see it in this silent video:

[demo]: /assets/blog/0.18/todomvc.html

"""


content = """

The video shows us starting our session in Chrome, testing all the features of our app. When we find that the ✖ button does not work, we press export. We then import that exact session in Safari to see if the behavior is the same. It is, so we can open a bug report with the *exact* exported session!

This blog post also marks the release of Elm 0.18, so from here we are going to (1) get into the technical details of the new debugger and (2) outline some of the other nice things that come with 0.18, like some error message improvements. I hope this stuff makes your life easier, and I am very curious to hear the first stories of QA teams using the debugger!

> **Note:** Install 0.18 from [here][install], and be sure to read the [migration guide][upgrade]! The latest `elm-format` will do a lot of the migration automatically, so check it out as well.

[install]: https://guide.elm-lang.org/install.html
[upgrade]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md


# Designing for Real Life

Elm first got into time-travel debugging with Laszlo Pandy’s work [in 2013](https://www.youtube.com/watch?v=lK0vph1zR8s). It made for a great demo, but it was never quite right for large projects. So Elm 0.18 came from the question: **what *exactly* do people using Elm in production want in a debugger?** I asked around and made a couple observations:

  - **Import/export is *the* feature.** Especially if you work with a QA team. It means you can quickly scan through the session history and see that they are getting bad data from the server. If so, you can skip the multi-hour wild goose chase and pass it along to the backend people.

  - **Debugging must work when Elm is embedded in JS or HTML.** Many folks who use Elm in production [embed it][embed] in a larger page. That means the debugger must work with JavaScript code that *cannot* time-travel safely. You can rewind your Elm program, but your JavaScript and database are not going to come along!

[embed]: /blog/how-to-use-elm-at-work
[mario]: https://www.youtube.com/watch?v=RUeLd7T7Xi4

But what about the cool stuff? What about seeing Mario’s position over time [as the code changes][mario]? It turns out the features that demo well are not so important for baseline production usage. By dropping features like that *for now*, I could focus on a more specific problem and get folks a useful tool sooner! So my constrained goal for the debugger was: get import/export working and make it lovely to use.


## Ease of Use

To use the debugger, you compile with the `--debug` flag. Like this:

```bash
elm-make Main.elm --debug
```

That’s all there is to it, even if you are embedding Elm in a larger page. I expect some folks will want to *always* add this flag in their dev builds, and I am excited to hear what that is like. Debug mode is also on by default with `elm-reactor`, so in that setting, you do not need to do anything!


## Reliability

Elm is known for having [nice error messages][errors], and the debugger is no exception. It will give you helpful feedback if you find yourself in a tricky situation. For example, say you export a session history, but by the time it gets to a developer, the program has changed a bit. Maybe some messages have been renamed or removed in a refactor? Elm can figure this all out at compile-time, so it kindly explains *why* the history is incompatible:

[errors]: /blog/compilers-as-assistants

"""

afterError = """

This seems reasonable. Maybe even boring. Obviously it should work this way! But imagine replicating this in JavaScript. With JS, the only way to know if the history is compatible is to try it out. If it is incompatible, it will throw a runtime exception. Not the runtime exception you are looking for, but a new one. One *created* by your debugging tools. Now maybe you are chasing after bugs that do not even exist!

Elm does not have this problem, not because of engineering, but because of the language itself. Elm is *designed* so that programs can be analyzed quickly and reliably, in ways that are [impossible][] in JavaScript. So as tooling in Elm continues to improve in the next releases, I am excited to see what other unique advantages we get thanks to the design of Elm itself!

[impossible]: https://en.wikipedia.org/wiki/Undecidable_problem


## More than a Debugger

Debuggers often feel like tools for experts, but I think ours can help total beginners too! The Elm debugger visualizes the program itself. Your code is no longer a mysterious black box that spits out pixels in the browser. Instead of trying to figure it all out in your head, you can actually *see* the messages come in and *watch* the model evolve!

I am pretty certain the debugger will help beginners understand The Elm Architecture, but I am excited to see how far it can go as a learning tool. Whether you are a beginner or expert, *seeing* the program helps you *understand* the program!


# What else is in Elm 0.18?

In addition to the debugger, there are some improvements to the error messages and core libraries. I will just be highlighting the most important stuff in this post, so check out the [migration guide][upgrade] for more details!

[upgrade]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md


## Improved Error Messages

Elm has an [error message catalog][catalog], full of messages that folks thought were confusing for some reason. This is great for constantly improving our error messages! Based on the feedback there, we have a couple nice improvements. Click the links to see examples!

  - [Bad Recursion][e1] &mdash; clears up confusion about mutation
  - [Type Annotations][e2] &mdash; improves hints when you add type annotations
  - [Missing Arguments][e3] &mdash; points out when you forget arguments
  - [Record Precision][e4] &mdash; makes hints very specific for large records
  - Preserve Names &mdash; shows `Html msg` instead of `VirtualDom.Node a`

[catalog]: https://github.com/elm-lang/error-message-catalog/
[e1]: https://gist.github.com/evancz/4589b798ac2475d52b1d00cd04ace2c8#bad-recursion
[e2]: https://gist.github.com/evancz/4589b798ac2475d52b1d00cd04ace2c8#type-annotations
[e3]: https://gist.github.com/evancz/4589b798ac2475d52b1d00cd04ace2c8#missing-arguments
[e4]: https://gist.github.com/evancz/4589b798ac2475d52b1d00cd04ace2c8#record-precision

These are definitely nice for experts, but I am most excited to see folks learning Elm with these additional hints! Recently, at the first ever [ElmBridge](https://twitter.com/elmlangbridge) event, I saw students running into problems that made me think “Oh, that is tricky, I’m gonna have to help.” But again and again, I was surprised to see that they just figured it out based on the error messages! My intuitions from my TA days and teaching older versions of Elm are not right anymore. Pretty exciting to see!


## Less Syntax

Elm is already quite a small language, but there are a few oddities that seem to do more harm than good in practice. So rather than saying “Oh well!” and keeping them forever, we are just taking them out:

  - **Primes** &mdash; Names like `x'` are no longer permitted. A younger me (one who was less concerned about nice variable names) certainly thought writing “x prime” was pretty neat! But in general, this syntax is too confusing to be worth it. Single quotes are generally associated with strings and characters. To see it unbalanced and part of a *variable* throws people off, and the benefit of having it is pretty small.

  - **Interpolation** &mdash; The `[1..5]` syntax was removed in favor of [`List.range`][range]. The syntax was kind of nice, but not very discoverable or commonly used. Whenever I used `[1..5]` in a talk, someone quite experienced would comment that they wanted something like that but could not find it in the standard libraries!

  - **Backticks** &mdash; ``buyMilk `andThen` dipCookie`` used to mean the same thing as `andThen buyMilk dipCookie`. This was in Elm since the beginning, so problems like “backticks look like single quotes” were overlooked. More recently, we noticed that everything nice about backticks is nicer with [the `|>` operator][pipe]. By flipping the arguments to `andThen`, we can write `buyMilk |> andThen dipCookie`. This reads nicely, it is easy to chain, it works well with `onError`, *and* it does not require any special syntax. It just builds on existing knowledge!

[pipe]: http://package.elm-lang.org/packages/elm-lang/core/4.0.5/Basics#|&gt;

Again, all these cases are covered in more detail in [the migration guide][upgrade], and `elm-format` will handle most of these cases automatically. Folks using the 0.18 alpha reported that the upgrade was quite smooth, so feel free to ask for help on [the Elm slack][slack]. The Elm community is friendly and happy to help!

[range]: http://package.elm-lang.org/packages/elm-lang/core/5.0.0/List#range
[alt]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md#backticks-and-andthen
[slack]: http://elmlang.herokuapp.com/


# Thank You

Huge thank you to [Pete Vilter](https://github.com/vilterp) who worked on Elm Reactor with me last summer! It took a while, but the ideas of writing as much as possible in Elm and splitting out a debug window are finally in there.

Thank you to everyone submitting [SSCCE](http://sscce.org/)’s to the [error message catalog][catalog]! It has been really exciting to see how direct feedback like this has improved things since we started this process in Elm 0.16.

Thank you to everyone who tried out the alpha and beta releases, reported bugs, and gave feedback on potential changes. Getting this kind of feedback is really important!

Finally, thank you to [Richard Feldman](https://twitter.com/rtfeldman) for getting the `npm` installer working, and thank you to [Aaron VonderHaar](https://twitter.com/avh4/) for updating `elm-format` to do a lot of this upgrade automatically!

"""

