import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Blog
import Center



(=>) = (,)


main =
  Blog.blog
    "Blazing Fast HTML"
    "Round Two"
    Blog.evan
    (Blog.Date 2016 8 30)
    [ Center.markdown "600px" intro
    , image "everyone"
    , Center.markdown "600px" afterEveryoneGraph
    , image "generalize"
    , Center.markdown "600px" afterGeneralizeGraph
    , image "elm"
    , Center.markdown "600px" postElm
    ]



-- IMAGES


image name =
  div [ Center.style "800px", class "content" ]
    [ a
        [ href "https://evancz.github.io/react-angular-ember-elm-performance-comparison/"
        , title "Run the benchmarks yourself!"
        ]
        [ img [ src ("/assets/blog/virtual-dom-charts/" ++ name ++ ".png") ] []
        ]
    ]



-- CONTENT


intro = """

Two years ago, we released an HTML rendering library for Elm, and it was
[really fast](/blog/blazing-fast-html). Faster than React, Angular, and
Ember. All these frameworks have improved their renderers since then, so
I figured it would be interesting to run the numbers again:

"""

afterEveryoneGraph = """

One big takeaway is that **Elm is the fastest**. When comparing React and Elm
in particular, you see that React takes about 50% longer to do the same work. Perhaps
even more interesting, Elm is extremely fast **even without optimization**. Just
by using Elm, you get performance React and Angular 1 do not achieve even with
hand-optimization.

Now, it is hard to create benchmarks that really show a fair comparison, so the
rest of this post is dedicated to exploring:

  - [How easy is optimization with each framework?](#ease-of-optimization)
    It is great if you can do well on benchmarks, but how difficult is it to
    actually do the optimizations in practice? In some cases the changes are
    easy and non-invasive. In other cases, you end up rewriting business logic.

  - [Is the methodology fair?](#methodology) The methodology we use actually
    handicaps Elm quite severely. I had to remove some optimizations from Elm
    itself to make things fair.

  - [Do these results generalize?](#do-these-results-generalize-) Elm is
    fast with TodoMVC, but what about in a *serious* project? Are these numbers
    representative of huge apps? In short, yes.

  - [What are the technical decisions that make Elm the fastest?](#technical-details)
    I did a full rewrite of our virtual DOM implementation for Elm 0.17, so we
    will get into the techniques I used to make things so fast.

If this gets you interested in Elm, start working through [the guide][guide]
or read about [how folks use Elm at work][work]. See if you like it! Otherwise,
I hope the information in this post will be helpful to all these different
communities.

[guide]: http://guide.elm-lang.org/
[work]: /blog/how-to-use-elm-at-work


> **Note:** I had a bunch of folks review this post before sharing it, and we
observed that the results are [quite consistent][graphs] across browsers,
operating systems, and hardware. If you want to check our work, [run the
benchmark yourself][run]. Let us know if you see something weird [here][repo]
and we&rsquo;ll try to fix it.

[run]: https://evancz.github.io/react-angular-ember-elm-performance-comparison/
[repo]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/issues
[graphs]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/tree/master/graphs


## Ease of Optimization

Being fast is nice, but **if no one can figure out how to get from naive code to
optimized code in a real app, the benchmarks do not matter.**

In the course of getting all these implementations set up, I was able to get
a feel for the ease of optimization with each project. How much work do I need
to do to make things faster? How hard do I need to think? How much do I need to
learn? What impact does it have on the code? **If you are serious about building
large projects, you should be asking yourself these questions.**

Before getting into the code, it is important to know that Elm, React, and
Angular 2 all use a virtual DOM strategy under the hood. That means they all
use two main optimization strategies:

  - **Skip Work** &mdash; The best way to speed up virtual DOM diffing is to
  skip it! So the programmer can say &ldquo;only diff the view if the relevant
  model changed&rdquo; with `lazy`, `shouldComponentUpdate`, or `OnPush`
  respectively.

  - **Align Work** &mdash; When you are adding and removing child nodes, a
  virtual DOM implementation can end up doing tons of pointless diffing. To
  avoid this you have `Html.Keyed`, `key`, or `trackBy` respectively.

So optimizing looks a bit different in practice with each of these projects, but
ultimately, they are enabling exactly the same optimizations behind the scenes.

Armed with that knowledge, let&rsquo;s do some analysis. I set up the benchmark
repo so that you can check out individual commits that show exactly how we
optimized each implementation:

[Elm]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/commit/afb72374a1b202760d209491f0540ba97e449b2e
[React]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/commit/a8052c6e1ffb0aedd77ab763fa644aa8c5a2253f
[Angular 1]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/commit/76bad72cc25fd1111bed9d06463a36c90eedb70b
[Angular 2]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/commit/49be6b269aaea259b8185ce02f1496d4f09100cd
[bad-angular]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/commit/9b29a440324a4d4b872eed6b89ca585edce34df6

  - **[Elm][]** &mdash; Optimizing Elm code means using `Html.Keyed` for the
  list of todo entries and sprinkling in `lazy` in strategic places. Very easy!
  More importantly, you are not changing your architecture or rewriting with
  different abstractions. You have a simple, effective toolkit that lives in
  your `view` code and does not disrupt your business logic.

  - **[React][]** &mdash; Here we add a `key` to each entry and define
  `shouldComponentUpdate` for the `Todo` component. Essentially the same as
  what we did in Elm, but we have to write some logic to figure out if a `Todo`
  changed. This opens us up to some extremely tricky bugs. Say you add
  `this.prop.checked` without changing `shouldComponentUpdate`. Now you either
  have a sneaky performance regression or a view that does not update correctly!
  Also notice that the React way is tied to components. Instead of sprinkling
  `lazy` around, a React app would require splitting things into tinier and
  tinier components to get the same benefits. That is totally possible, but now
  we are talking about pretty serious architecture changes for the sake of
  performance.

  - **[Angular 1][]** &mdash; Here we [stop using `$scope.$watch` entirely][watch].
  This requires changes in our business logic, not just in our view code like
  in React and Elm! The speed gain is good, but now there is a risk that we
  forget this line when adding new business logic. So like React, we just have
  to hope that no one ever messes it up ever. Or that our tests and QA always
  catch it. This optimization also seems quite counter-intuitive. The way to
  make it faster is to completely avoid parts of the framework.

[watch]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/blob/master/implementations/angular-1.5.8-optimized/readme.md

  - **[Angular 2][]** &mdash; In this change we just start using `trackBy`.
  This is the equivalent of `Html.Keyed` in Elm and `key` in React. It *looks*
  very simple, but notice that we do not start using `OnPush`. The standard
  TodoMVC implementation did not have a `Todo` component, so we had to do
  [quite a serious refactor][bad-angular] to be able to use it, even touching
  the store! (React is just as bad if the thing you want to avoid building is
  not already a component. Changing architecture for optimization!) Once all
  that is done, you can add `OnPush` to the `Todo` component, but with one huge
  caveat. If you do any mutation in your view when `OnPush` is enabled, you
  will get the same kind of bugs that React&rsquo;s `shouldComponentUpdate`
  allows: sneaky performance regressions and invalid views.

  - **Ember** &mdash; We could not figure out how to optimize this one. I assume
  it is possible, but the fact that we could not figure it out is interesting in
  its own right. If anyone knows how, I would be interested to learn more about
  that!

[Stephen]: https://github.com/stephencelis

My analysis is that **optimization is easier and more reliable in Elm** than
in the others. Things are relatively easy with React and Angular 2, but (1) you
end up having to rearchitect your application to get the benefits of
`shouldComponentUpdate` and `OnPush` and (2) both `shouldComponentUpdate` and
`OnPush` leave you open to bugs that are extremely tricky to track down. The
first problem is unavoidable, but the second one can be mitigated in various
ways. In both React and Angular 2 you can use an immutability library to be more
certain that there is no funny business going on. I would add that to my list
of dependencies and hope that no one on my team ever made a mistake and mutated
something by accident. Angular 2 goes a step farther. They have a &ldquo;dev
mode&rdquo; that runs over your entire store to make sure nothing got changed
by view code. It is quite a nice feature, but if you mutate something outside
of your store or do any side-effects, it cannot catch that. Even paired with
testing and QA, **none of these mitigation techniques are as reliable as Elm
out-of-the-box.** These problems just cannot happen in Elm, and in the end, Elm
apps are faster anyway.

> **Note:** In the React version, we added the `key` attribute *outside* the
`Todo` component. You could just as easily add it inside though. And if it was
inside, the `Todo` component would be keyed no matter where you use it, right?
Actually, the reverse is true: it would *never* be keyed. Using
`shouldComponentUpdate` means we do not build anything, but if `key` is inside,
we do not build that either. In other words, you end up *hiding* the key such
that it is never used! Simple mistake, but pretty bad consequence. This
contrasts with Elm and Angular 2 which make this kind of problem impossible.


# Methodology

My goal with these benchmarks was to compare renderer performance in a
realistic scenario. This means rendering each frame in full, exactly like
you would if a real user was interacting with the TodoMVC app. I achieved
this with two rules:

  - **No Batching Events** &mdash; Instead of generating events in a `for` loop,
  our user simulator generates events one at a time, waiting for the resulting
  frame to be rendered. Without this, it *looks* like Elm is 3x or 8x faster than
  some of the competitors, but that is not under &ldquo;normal&rdquo; circumstances.

  - **No `requestAnimationFrame`** &mdash; Elm uses `requestAnimationFrame`
  by default, allowing us to skip frames users would not see anyway. That means
  Elm is great at animation out-of-the-box, but none of the other projects do
  this by default. Unfortunately for Elm, the whole point of this benchmark is
  to compare the *renderers*, not figure out who can safely skip the renderer
  entirely! I removed this logic from the Elm runtime by hand, hurting Elm, but
  putting everyone on more equal footing.

You can read a ton more about these rules [here][methodology]. It goes into
what &ldquo;batching events&rdquo; really means and why it makes Elm look a lot
faster. It also explains why React, Ember, and Angular would not enable
`requestAnimationFrame` by default even if they wanted to. I think it is all
interesting, but I heard it made this blog post drag.

[methodology]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/#methodology


# Do these results generalize?

As I mentioned in the [Ease of Optimization](#ease-of-optimization) section,
React, Elm, and Angular 2 all use a virtual DOM strategy under the hood. That
means that the two major optimization strategies are **skip work** and
**align work**.

Point is, comparing these projects without these tricks is actually a pretty
fair assessment of the underlying virtual DOM implementation. You are building
the *entire* virtual DOM, diffing *everything*, and then going and making your
patches. This strips away optimization done by the programmer and leaves us
with the questions: who builds virtual DOM faster? And who diffs it faster? So
let&rsquo;s see the unoptimized versions side-by-side:

"""


afterGeneralizeGraph = """

This graph is why I think these performance numbers generalize to all apps.
Sure, this is a TodoMVC app that is not too complicated, but we are comparing
the core diffing algorithm here. Who builds nodes faster? Who diffs faster?
Each project let you skip as much of that as possible (using the same techniques
behind the scenes) but ultimately you have to run this code. **So as your
app gets bigger, the speed difference can only get bigger.**

> **Note:** To folks who think the React implementation is not representative
because it is not using a certain library, the README from todomvc.com
[addresses this directly][rep]. Whether you are using Redux or Immutable.js or
whatever else, the fundamental performance characteristics of *rendering* with
React are the same. No matter how you combine libraries, you will eventually
need to build virtual DOM nodes and diff them.

[rep]: https://github.com/evancz/react-angular-ember-elm-performance-comparison/tree/master/implementations/react-15.3.1-optimized#react-todomvc-for-benchmarking


# Technical Details

Okay, so the Elm implementation is very fast, but why?

Until recently we were using `MattEsch/virtual-dom` as our underlying
implementation. It is pretty nice, but I needed to rewrite from scratch to
support an API change in Elm 0.17 that made things significantly easier to
learn. The following graph compares the old and new implementations:

"""


postElm = """

Things are quite a lot faster! I think this is mainly attributable to the
following techniques:

  - **Prefer arrays over dictionary objects.** Crawling an array is much faster
  than crawling an object. `for (var key in object)` is just never going to be
  as fast as `for (var i = 0; i < len; i++)`. Whenever there is a dynamic number
  of things, find a way to allocate them one-by-one at the end of an array.

  - **Do not allocate.** A big cost in these implementations is garbage
  collection. The fewer objects you allocate the better. So one trick we used
  was allocating objects with nulled out fields that we would fill in later.
  This does two things. First, it means the *shape* of our object stays the
  same over time, making it easier for JavaScript engines to optimize them.
  Second, it means we can keep using the same array even as we gather more
  information.

  - **Never look anything up**. This means avoiding `object[key]` or `array[i]`
  as much as possible. We only want to crawl arrays in order. You can often
  avoid lookups with creative use of references. So instead of looking
  something up in an object, what if you already had a reference to what you
  wanted?

I was happy to see that common sense strategies paid off so well. Use the
fastest data structure possible. Avoid slow operations. Avoid operations in
general. Think about allocation and garbage collection. Nothing crazy really,
but hopefully it will be helpful to folks nonetheless!


# Conclusion

Benchmarking is difficult, but hopefully I have made a convincing case that:

  - Elm is very fast.
  - Optimizing Elm only touches `view` code, unlike everyone else.
  - Optimizing Elm cannot introduce sneaky bugs, unlike everyone else.
  - These results should generalize to apps of any size.

That is all great, but this is kind of selling Elm short. We also have a
compiler that gives [extraordinarily helpful hints](/blog/compilers-as-assistants)
that prevents runtime errors. It is good enough that NoRedInk is running 36k
lines of Elm in production and has never gotten a runtime error from their Elm
code in more than a year of use. (Rollbar reports everything, and it is always
from JS!)

I know a lot of people think, &ldquo;that seems nice, but it is a whole
different *language*. I cannot rewrite a project entirely!&rdquo; That is
absolutely true, so folks who end up using Elm in production introduce it
gradually, [like this][work]. So if you are interested,
[get Elm installed](/install) and check out [the guide][guide]. Try making
something small, and if you run into any problems, come talk to folks in the
Elm community! We are friendly and happy to help out. The [Elm Slack][slack]
and [mailing list][elm-discuss] are both great places to ask questions, and
you can often save yourself hours or days of [XY problems][xy] by asking a
human.

Whatever you end up doing, I hope you learned something from this post! I
certainly learned a ton from writing up the &ldquo;Ease of Optimization&rdquo;
section about the concrete tradeoffs between each of these different projects,
so I hope that breadth of experience can help folks make technical decisions
without doing all this work!

[guide]: http://guide.elm-lang.org/
[work]: /blog/how-to-use-elm-at-work
[slack]: http://elmlang.herokuapp.com/
[elm-discuss]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss
[xy]: http://meta.stackexchange.com/questions/66377/what-is-the-xy-problem


# Thank You

I want to give a huge thank you to [Sergey][] for his work on both Angular
implementations! I definitely could not have done the optimization without
you, and I learned a bunch from how you did things! I also want to thank
[Stephen][] who contributed the Ember implementation. Holy cow, so many files!

[Sergey]: https://github.com/IwalkAlone
[Stephen]: https://github.com/stephencelis

"""
