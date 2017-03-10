import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Blog
import Center



(=>) = (,)


main =
  Blog.blog
    "Blazing Fast HTML"
    "Virtual DOM in Elm"
    Blog.evan
    (Blog.Date 2014 7 23)
    [ Center.markdown "600px" content ]


content = """

The new [elm-html][] library lets you use
HTML and CSS directly in Elm. Want to use flexbox? Want to keep using existing
style sheets? Elm now makes all of this pleasant and *fast*. For example, when
recreating the [TodoMVC][todo] app, the [code][code] is quite simple and our
[preliminary benchmarks][bench] show that it is extremely fast compared to other
popular entries:

[elm-html]: https://github.com/elm-lang/html
[todo]: http://evancz.github.io/elm-todomvc/
[code]: https://github.com/evancz/elm-todomvc/blob/master/Todo.elm
[bench]: https://evancz.github.io/todomvc-perf-comparison

<a href="https://evancz.github.io/todomvc-perf-comparison">
<img src="/assets/blog/virtual-dom-charts-old/sampleResults.png"
     alt="sample results with Firefox 30 on a Macbook Air with OSX 10.9.4"
     title="sample results with Firefox 30 on a Macbook Air with OSX 10.9.4"z
     style="max-width: 500px; margin-left: auto; margin-right: auto; display:block;"></a>

Both [elm-html][] and Mercury are based on the [virtual-dom][] project, which is
responsible for the great performance. The first half of this post will explore
what &ldquo;virtual DOM&rdquo; means and how **purity** and **immutability**
make it extremely fast. This will explain why Om, Mercury, and Elm all get such
great numbers.

[virtual-dom]: https://github.com/Matt-Esch/virtual-dom

Performance is a good hook, but the real benefit is that this approach leads to
code that is easier to understand and maintain. In short, it becomes very simple
to create reusable HTML widgets and abstract out common patterns. *This* is why
people with larger code bases should be interested in virtual DOM approaches.

This library is also great news for people who have been thinking about using
Elm. It means you can use Elm *and* keep using the same CSS and
designer/developer workflow that you are comfortable with. It is simpler than
ever to get the benefits of Elm in your project. Let&rsquo;s see how it works.

## Virtual DOM

This library is based on the idea of a &ldquo;virtual DOM&rdquo;. Rather than
touching the DOM directly, we build an abstract version of it on each frame. We
use the `node` function to create a cheap representation of what we want:

```elm
node : String -> List Attribute -> List Html -> Html
```

This lets us specify a tag, a list of HTML attributes, and a list of children.
For HTML5 tags, there are helper functions such as `(div = node "div")` to make
things look a lot cleaner. For example, here is how you can build a simple
`profile` widget that shows a user&rsquo;s picture and name:

```elm
profile : User -> Html
profile user =
    div [ class "profile" ]
      [ img [ src user.picture ] []
      , span [] [ text user.name ]
      ]
```

Notice that we set a class so the whole thing can be styled from CSS. Paired
with Elm&rsquo;s module system, this makes it easy to abstract out common
patterns and reuse code. You can check out the full API and documentation
[here](http://package.elm-lang.org/packages/evancz/elm-html/latest/) and we will
explore more example uses soon in the section on [reusable
widgets](#reusable-widgets).

## Making Virtual DOM Fast

Virtual DOM sounds pretty slow, right? Create a whole new scene on every frame?
This technique is actually [widely used in the game industry][scene] and
performs shockingly well for DOM updates when you use two relatively simple
techniques: diffing and laziness.

[scene]: http://en.wikipedia.org/wiki/Scene_graph

React popularized the idea of &ldquo;diffing&rdquo; to figure out how the DOM
needs to be modified. **Diffing means taking the *current* virtual DOM and the
*new* virtual DOM and looking for changes.** It sounds kind of fancy at first,
but it is a very simple process. We first make a big list of all the
differences, like if someone has changed the color of a particular `<div>` or
added an entirely new one. After all of the differences are found, we use them
as instructions for modifying the DOM in one big batch using
[`requestAnimationFrame`][raf]. This means we do the dirty work of modifying
the DOM and making sure everything is fast. You can focus on writing code
that is easy to understand and maintain.

[raf]: https://developer.mozilla.org/en/docs/Web/API/window.requestAnimationFrame

This approach created a clear path to fully supporting HTML and CSS in a way
that is perfect for Elm! Even better, Elm already has great facilities for
purity and immutability, which are vital for optimizations that make diffing
*way* faster.

As pioneered by React and Om, being *lazy* about diffing can lead to great
performance improvements, especially if you have immutable data. For example,
let&rsquo;s say we are rendering a list of tasks:

```elm
todoList : List Task -> Html
todoList tasks =
    div [] (map todoItem tasks)
```

But we may know that on many updates, none of the tasks are changing. And if no
task changes, the view must not be changing either. This is a perfect time to be
`lazy`:

```elm
lazy : (a -> Html) -> a -> Html


todoWidget : State -> Html
todoWidget state =
    lazy todoList state.tasks
```

Instead of calling the `todoList` function on every frame, we check to see if
`state.tasks` has changed since last frame. If not, we can skip everything.
No need to call the function, do any diffing, or touch the DOM at all!
This optimization is safe in Elm because functions are [pure][] and data is
[immutable][].

  * **Purity** means that the `todoList` function will *always* have
    the same output given the same input. So if we know `state.tasks` is the same,
    we can skip `todoList` entirely.

  * **Immutability** makes it cheap to figure out when things are &ldquo;the
    same&rdquo;. Immutability guarantees that if two things are referentially
    equal, they *must* be structurally equal.

So we just check to see if `todoList` and `state.tasks` are the same as last
frame by comparing the old and new values by *reference*. This is super cheap,
and if they are the same, the `lazy` function can often avoid a ton of work.
This is a pretty simple trick that can speed things up significantly.

[pure]: http://en.wikipedia.org/wiki/Pure_function
[immutable]: http://en.wikipedia.org/wiki/Immutable_object

If you have been following Elm, you may begin to see a pattern:
purity and immutability are kind of a big deal. Read about [hot-swapping in
Elm](/blog/interactive-programming) and the [time traveling
debugger](http://debug.elm-lang.org/) to learn more about this.

## Reusable Widgets

This approach makes it incredibly simple to create reusable widgets. For
example, a list of user profiles can be nicely abstracted with something like
this:

```elm
import Html exposing (..)


profiles : List User -> Html
profiles users =
    div [] (List.map profile users)


profile : User -> Html
profile user =
    div []
      [ img [ src user.picture ] []
      , text user.name
      ]
```

We now have a `profiles` widget that takes a list of users and gives us back
some HTML. It is easy to reuse anywhere, and unlike templating languages, we can
use any part of Elm to help create widgets like this. We can even begin to
create community libraries for common widgets or patterns.

If you want to create complex styles, those can be abstracted out and reused
too! In the following example, we define a `font` and `background` that
can be mixed and matched on any node.

```elm
-- small reusable CSS properties
font : List (String, String)
font =
    [ ("font-family", "futura, sans-serif")
    , ("color", "rgb(42, 42, 42)")
    , ("font-size", "2em")
    ]


background : List (String, String)
background =
    [ ("background-color", "rgb(245, 245, 245)")
    ]


-- combine them to make individual nodes
profiles : List User -> Html
profiles users =
    div [ style (font ++ background) ] (List.map profile users)
```

So creating reusable widgets and abstracting out common patterns is extremely
simple now, but we can do much more than this!

## Freedom of Abstraction

When I started working on the project that would become Elm, HTML was about 20
years old and people still had to read three blog posts and five Stack Overflow
questions to figure out how to vertically center things. My initial goal with
Elm was to rethink GUIs from scratch. **What would web programming look like if
we could restart?**

[elm-html][] has two very important strengths in pursuing that goal. First, it
gives you access to HTML and CSS, so you can always take full advantage of the
latest features. Second, it makes it possible to create *new* abstractions.

This means **HTML and CSS become the basic building blocks for *nicer*
abstractions.** For example, it may be possible to recreate Elm&rsquo;s
`Element` abstraction using this library. But most importantly, *anyone* can
experiment with new ways to make views more modular and pleasant. Paul Chiusano
explains this aspiration very nicely in his [provocative post on CSS][css].

[css]: http://pchiusano.github.io/2014-07-02/css-is-unnecessary.html

My goal with Elm is still to rethink web programming, and in a weird and twisted
way, fully supporting HTML and CSS is a big step in that direction. I am excited
to see what we can do with [elm-html][]!

## Notes on Architecture

As with any new approach, one of the first questions people ask is &ldquo;what
does it look like for a large project?&rdquo; The general approach is in the
same ballpark as architecting large applications with Om or Facebook’s Flux. I
have informally outlined [how this works in Elm][architecture] and plan to
create more formal documentation and examples soon.

[architecture]: https://gist.github.com/evancz/2b2ba366cae1887fe621

## Thank you

Thank you to React and Om for discovering and popularizing these techniques.
Thank you in particular to Sebastian Markbage, David Nolen, Matt Esch, and Jake
Verbaten who helped me *understand* them.

Huge thanks to Matt Esch and Jake Verbaten who created [virtual-dom][] and
[mercury][], which this library is based on. They are fully responsible for
the great performance!

[mercury]: https://github.com/Raynos/mercury

"""
