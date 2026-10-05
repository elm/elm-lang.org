import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Skeleton
import Center


main =
  Skeleton.news
    "Another step towards Elm 1.0"
    "Making the compiler “correct by construction”"
    Skeleton.evan
    (Skeleton.Date 2026 10 5)
    [ Center.markdown "600px" content
    ]


content : String
content = """

People love Elm for the friendly error messages, easy refactoring, and strong correctness guarantees. Many people have such a nice time writing their frontend code with Elm that they end up wanting the same level of quality in their backend code as well. So we have been hard at work expanding the “the Elm experience” to the server and database in a thoughtful and coherent way. Check out [Acadia](https://acadia.engineering/) and [`elm-simple-server`](https://github.com/acadia-engineering/elm-simple-server) if you are interested in that! Acadia is also the “Patreon” for Elm, so we are also on the way to a stable and sustainable financial foundation. (Thank you! This work is not possible without your [support](https://acadia.engineering/support)!)

Today marks another step on the road to “the end-to-end Elm experience” with the second incremental Elm release. You can get the 0.19.3 binaries [here](https://github.com/elm/compiler/releases/tag/0.19.3)!

The rest of this post gets into (1) the rough roadmap for the next few Elm releases and (2) the infrastructure improvements in Elm 0.19.3 which focus on making the compiler “correct by construction”. These “correct by construction” techniques are useful in any program, so hopefully you will be inspired to improve your own code with these ideas!


## The Rough Roadmap

The compiler for [Acadia](https://acadia.engineering/) is significantly more advanced than the Elm compiler, so we need to backport some infrastructure improvements to get them more aligned (blue changes). This will unlock “more exciting” improvements down the line (black changes).

![feature tree](/assets/blog/0.19.3/feature-tree.svg)

I am quite excited about the projects that will be possible after we line up the two compilers! **We are also experimenting with a faster release cycle**, so the current plan is to break the infrastructure changes (in blue) into a couple of smaller releases.

Now on to the details of the 0.19.3 release!


## Better Types for Better Programs

Elm takes the perspective that our programs should be “correct by construction” such that invalid data is not even representable. **Entire categories of “invalid data” bugs can be eliminated when you use custom types to guarantee that such data simply cannot exist.** This concept has been called “make invalid states unrepresentable” or “make impossible states impossible” and it is great for code quality.

To that end, many of the changes in Elm 0.19.3 focus on using more precise types in the compiler. We will focus on two little examples that may be inspiring to people interested in functional programming techniques.


### A `String` is not always a `String`

In Elm, it is common to create a unique type for certain kinds of data. For example, if you want a 100% guarantee that you are working with a validated email address, you can create a module like this:

```elm
module Email exposing (Email, validate, toString)

type Email = Email String

validate : String -> Maybe Email

toString : Email -> String
```

Now the *only* way to construct an `Email` value is to call `Email.validate`. You now have a 100% guarantee that all `Email` values have been validated!

This also means that you can never accidentally use an `Email` as a regular `String`. These are two distinct types. This is great as you start working with concepts like `UserName` or `LastName` because now you have a 100% guarantee that you never use a `LastName` as a `UserName`. **This category of bug can be fully eliminated!**

**Elm also imposes zero runtime overhead for creating `Email` values.** The Elm compiler recognizes that it is just a wrapper around the `String` type, so it uses a plain `String` in the generated JS code.

This technique gives you strong guarantees that can be useful in all sorts of programs! It is extremely nice in my Acadia databases, where my `Email` columns are 100% guaranteed to be separate from my `UserName` columns. It is even useful for compilers! In the old Elm compiler, every variable name was represented by a `Name` type like this:

```elm
type Name = Name String
```

This is an improvement over using a plain `String`, but there are a couple different categories of names in Elm. There are type names, module names, module prefixes, variable names, type variable names, and operator names. Each has their own rules and they should never be mixed. So in Elm 0.19.3 each of these categories now is a distinct type. This makes the code a lot nicer to read, and it gives us a 100% guarantee that these names are never crossing over into incompatible categories.

When making these kinds of changes, there is a chance you uncover bugs that have been overlooked. Maybe a module name is getting used as a variable name in some weird scenario? **I actually discovered a pretty subtle bug during this refactor!** In some cases, a hand-written type alias must be “dealiased” and expanded into its underlying type, and in rare cases, the hand-written type involves an “extended record” that leaves some fields open as a type variable, and in even rarer cases, the hand-written type involves *nested* extended records which are possible-but-generally-recommended-against. Switching to a distinct `ExtVar` type made it clear that the extension variable was not being properly transformed in the nested case! We had known of the rather rare symptoms of this issue for a long time, but **the root cause was only revealed by an independent initiative to have a “correct by construction” compiler!**

I wonder if you can find similar bugs in your code with the same simple technique!


### A `List` that is never empty

Another nice example of data structures that are “correct by construction” is in how we detect module cycles. People using Elm in larger applications may have a couple hundred modules, and the compiler provides friendly error messages to help disentangle any cycles that emerge:

<pre><code class="lang-elm"><span class="hljs-type">-- IMPORT CYCLE ----------------------------------------------------------------</span>

Your module imports form a cycle:

    ┌─────┐
    │    <span class="hljs-name">User</span>
    │     ↓
    │    <span class="hljs-name">Post</span>
    │     ↓
    │    <span class="hljs-name">Blog</span>
    └─────┘

Learn more about why this is disallowed and how to break cycles here:
https://elm-lang.org/0.19.3/import-cycles</code></pre>

Our algorithm detects [strongly connected components](https://en.wikipedia.org/wiki/Strongly_connected_component), and it turns out that a strongly connected component (SCC) may not *always* be a nice linear cycle. Here are some examples of “difficult” strongly connected components:

![strongly connected components](/assets/blog/0.19.3/strongly-connected-components.svg)

Rendering these non-linear cyclic graphs in a clear way is not very easy, especially in the terminal and especially as the graphs get larger. Graphs with many nodes and edges often look very confusing when flattened into 2D. These sorts of cases are rare in normal programs, so for a long time, we did not realize that our cycle renderer was not showing them properly!

So we took the approach of making our `Graph` types more precise with an API like this:

```elm
module Graph exposing
  ( Node, SCC(..), Component, toStronglyConnectedComponents
  , MinimalCycle(..), toMinimalCycle
  )

type alias Node key value =
  { key : key
  , value : value
  , edges : List key
  }

toStronglyConnectedComponents : List (Node k v) -> List (SCC k v)

type SCC k v
  = Acyclic (Node k v)
  | Cyclic (Component k v)

type Component k v =
  Component (Node k v) (List (Node k v))

toMinimalCycle : (k -> v -> a) -> Component k v -> MinimalCycle a

type MinimalCycle a =
  MinimalCycle a (List a)
```

There is a lot to process here, so we will start with the `MinimalCycle` type at the very end. There are a couple of things we want to guarantee about these cycles:

  1. The cycles are never empty. There is always at least one module in a cycle.
  2. The cycles are always linear. The arrows we draw in the error messages should show the shortest path from one module back to itself.

For our first guarantee, we use a common technique for guaranteeing that “this list is never empty”. Notice that the `MinimalCycle` type requires that you provide one value, and then a list of additional values, like `MinimalCycle 1 [2,3]` or `MinimalCycle 1 []`. This makes it is impossible to create a `MinimalCycle` with zero entries! There must always be one value, and then the list with any additional values. **Now we have a 100% guarantee that our cycles are non-empty.** They are “correct by construction”.

The second guarantee is enforced by having strong module boundaries. Running `toStronglyConnectedComponents` will produce a cyclic `Component` value when there are module cycles, and these components may have all sorts of confusing edges that are difficult to render. We keep these details internal to the `Graph` module. if you want to see what is inside a `Component`, the only option is to call `toMinimalCycle` to convert it into a minimal linear cycle. There is no other way! **Now we have a 100% guarantee that we always render nice linear cycles in our error messages!** All code outside of the `Graph` module must convert a `Component` to a `MinimalCycle` if they want to look at the values.

Next time you run into a cycle in an Elm error message, perhaps you will be reminded of these techniques for getting strong guarantees in your own code!


## Conclusion

**When our programs are “correct by construction”, we rule out entire categories of bugs.** It is no longer a matter of testing or vigilance. The bugs are just not possible! The simple techniques we discussed here are useful whether you are writing websites, databases, or compilers, and I hope you will give them a try with the online [examples](https://elm-lang.org/examples) or with [Elm 0.19.3](https://github.com/elm/compiler/releases/tag/0.19.3) and the [guide](https://guide.elm-lang.org)!

In addition to the compiler changes described in this post, we also made improvements to our primitives for concurrency, binary serialization, and file locking. All the changes are within the overall theme of “correct by construction” such that guarantees like “file writes can only happen when you have a valid project lock” are enforced by the type system. These more precise primitives give us a strong foundation for the user-facing language work we have lined up.

**If you like what you are seeing, please consider supporting Elm and Acadia development [here](https://acadia.engineering/support)!** We write everything by hand with a focus on quality and performance, and we hope our tools help you do the same.

Finally, thank you to everyone using and supporting Elm and Acadia! We love writing beautiful and simple code, and we are glad you do too!

> **Note:** If you want to follow Elm news, I recommend [twitter](https://x.com/evancz) for the “big news” and [bluesky](https://bsky.app/profile/acadia.engineering) or [discourse](https://discourse.elm-lang.org/) for more frequent and more interactive updates. We try to make sure posts make it around on Reddit and Hacker News, but obviously not every post gets the distribution it deserves, like [this one](https://acadia.engineering/blog/simple-and-efficient-row-level-security) about row-level security and symbolic evaluation.

"""
