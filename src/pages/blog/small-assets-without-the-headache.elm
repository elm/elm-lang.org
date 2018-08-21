import Html exposing (..)
import Html.Attributes exposing (..)

import Skeleton
import Center


main =
  Skeleton.blog
    "Small Assets without the Headache"
    "Minification made easy with Elm 0.19"
    Skeleton.evan
    (Skeleton.Date 2018 8 21)
    [ Center.markdown "600px" part1
    , a (href "https://gist.github.com/evancz/6b3d9a26f7238912e01b25b1b5c68db1" :: Center.styles "740px")
        [ img [ src "/assets/blog/0.19/asset-sizes.png", style "max-width" "100%" ] []
        ]
    , Center.markdown "600px" part2
    ]


part1 = """

If you want faster page loads, you want smaller assets. **But it feels quite complicated to get small assets with JavaScript.** Should I switch from React to Preact? Do I have tree shaking set up properly? Will my dependencies even work with tree shaking? Do I need to do something special with my dependencies now? And will code splitting help as well? How does that work? It feels overwhelming even talking about it!

**Elm 0.19 makes our assets really small without the headache.** A bunch of projects have implemented this “RealWorld App” that is a decent size application. We compared some of the most popular implementations to see how Elm stacks up:

"""

part2 = """

The Elm version is quite small! (Smaller is better. Smaller assets means faster downloads!) Note that the React library itself is 32kb. Just the library without any application code. The entire [Elm RealWorld App](https://github.com/rtfeldman/elm-spa-example) is 29kb, so no amount of code splitting can make the React version smaller than the Elm version!

I was really excited when I first saw these results, but the RealWorld App is not _that_ big. The Vue implementation is only about 2000 lines. So what about larger projects? We had a community member working on a **49,315 line** application try the new version and he got **114kb** after compilation, minification, and gzip. That is in the ballpark of the 100kb produced by the 2000 lines in the Vue implementation! So it looks like your projects need to get exceptionally large before you start running into the _baseline_ issues you see in JavaScript.

The best part is that it is easy to get these results. No need to write your code in special ways or do tons of configuration. You just add the `--optimize` flag when you compile. The compiler takes care of everything else!

The rest of this post will explore (1) the new optimizations that make these results possible, (2) how the new compiler became extremely fast for large projects, and (3) some nice improvements and simplifications.

> **Note:** Check out the full [release notes](https://github.com/elm/compiler/blob/master/upgrade-docs/0.19.md) to see everything that is new in Elm 0.19. The installers are available in the freshly updated [official guide](https://guide.elm-lang.org)!


## Dead Code Elimination

The primary optimization in Elm 0.19 that gets us such small assets is **function-level dead code elimination**.

If you use a package with hundreds of functions, like `elm/html`, it automatically gets trimmed down to the 10 you actually use. The compiler can just figure it out. **This works across the entire ecosystem.** Using one function from a massive package with tons of dependencies? No problem. You just get that one function in the generated code.

This level of granularity is possible because:

- **Elm functions cannot be redefined or removed at runtime.** This makes it easy to tell if something is needed. Is the code reachable from `main`? If so, keep it. If not, drop it. Whereas in JavaScript, any module might modify `window.Array` or `Array.prototype`, changing how code _elsewhere_ evaluates. So the fact that you do not call it directly does not prove it can be dropped.
- **Every package on [`package.elm-lang.org`](https://package.elm-lang.org) is written entirely in Elm.** That means we have a 100% guarantee that there is nothing weird going on in any of your dependencies, so we can cut it up just as easily as your application. If packages contained arbitrary JavaScript code, we would inherit all the same optimization challenges and have to be more conservative.

In JavaScript, the equivalent of dead code elimination is commonly called [tree shaking](https://webpack.js.org/guides/tree-shaking/). As that link says, tree shaking generally works with a granularity of modules (not functions) due to the potential redefinition and removal of functions. The larger granularity means you are more likely to get modules you do not use. And those modules can bring in _more_ modules you do not use. Even if you do not use any of their functions directly, you might rely on the functions they mutate. This leads to a cascading increase in asset size as projects get more complicated:

![](/assets/blog/0.19/dce.svg)

This is further compounded by the fact that `npm` allows many versions of the same package in a single project. To work around all this, some JavaScript packages go so far as to chop themselves into individual functions, like [lodash-modularized](https://www.npmjs.com/search?q=keywords:lodash-modularized), so application developers have the _option_ to manually approximate what Elm does automatically. **So instead of sacrificing development time and code clarity to shave bits, Elm lets you just focus on making great packages and applications.** The compiler will take it from there!


## Record Field Renaming

When you use the `--optimize` flag, you get a couple extra optimizations. One interesting one is **record field renaming** across your whole codebase. This optimization turns long names like `student.mostRecentGrade` into `s.m` instead. It tends to give a 5% to 10% reduction in asset size.

Again, **this works across the entire Elm ecosystem** because every single package on `package.elm-lang.org` written completely in Elm. To acheive similar results in JavaScript, you must avoid `student['mostRecent' + info]` and statically figure out the difference between `student[field]` and `student[index]`. If you have ever tried to use `ADVANCED_OPTIMIZATIONS` in Google Closure Compiler, you know that this is extremely difficult even when you write all the code yourself, but it would have to work across the 700k `npm` packages out there to give comparable results.


## Compile Times

Now some readers may be worried that these optimizations come at the cost of slower compile times. Just the opposite! **The new compiler is quite fast!**

Earlier we mentioned a community member with **49,315 lines** of code spread across 227 modules. The application is called [SchoolHouse](https://schoolhouse.io/). The author compiled **from scratch** on his desktop and got the following output:

```bash
$ time elm make src/Main.elm --optimize
Dependencies loaded from local cache.
Dependencies ready!
Success! Compiled 227 modules.

real    0m1.959s
user    0m1.129s
sys     0m0.066s
```

**Under two seconds for 50k lines of code.** Pretty good, especially considering that this is the absolute worst case. The incremental compiles you do in development will only compile the files with potential changes, which should bring his times under 500ms for most changes.

I suspect this compares favorably with 50k line projects that use Babel, TypeScript, Flow, etc. but I would love to see data to do a concrete comparison. Please share on [Discourse](https://discourse.elm-lang.org/) if you have the data!

It took a lot of unglamorous work within the compiler to get this kind of performance (e.g. rewriting the parser) and I am particularly excited to hear how it helps companies like NoRedInk and Featurespace which both have more than 200k lines of Elm!


## Fun Stuff

I feel like a robot talking about all those numbers. That stuff is nice and all, but I also made improvements to packages, documentation, and error messages! My personal favorites include:

- Nicer parse errors. Should help a lot with the first few weeks of Elm!
- Better docs on types, interop, and “single page apps” in [the official guide](https://guide.elm-lang.org/).
- A simpler way to think about time and time zones in [`elm/time`](https://package.elm-lang.org/packages/elm/time/latest/).
- The [`getViewport`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#getViewport) family of functions for getting scroll positions.

These examples showcase the effort that has gone into continually making Elm simpler and friendlier, but there are a bunch of other improvements listed in [the release notes](https://github.com/elm/compiler/blob/master/upgrade-docs/0.19.md) in the same spirit!


## Conclusion

I am excited to finally share this release publicly! I hope it will help you out, whether you are learning your first programming language or on your way to 300k lines of code at work.

As folks with 50k+ lines upgrade to Elm 0.19, I encourage you to share your new asset sizes and compile times. You can send feedback to the core team directly to help explore further optimizations like code splitting as described [here](https://gist.github.com/evancz/fc6ff4995395a1643155593a182e2de7). And if you run that script and are excited about the results, please blog about it!

And finally, **if you decide to give Elm a try, start with [the official guide](https://guide.elm-lang.org/) and ask questions on [Discourse](https://discourse.elm-lang.org/) and [Slack](http://elmlang.herokuapp.com/) if you need help with anything.** We will do our best to help you out, but everyone has different projects and preferences. So even if you find it is not the right fit for you, I hope you will come away with a positive learning experience!


<br>

<br>

## Thank You

This was quite a tough release cycle, so thank you to everyone who was supportive and patient over the past 18 months. Thank you in particular to the community members who felt emotional strain alluded to in the beginning of [What is Success?](https://youtu.be/uGlzRt-FYto) I wish it never got like that, but it was also very encouraging to see so many people take time out of their day to be supportive online and in person. I learned a lot in this time!

Thank you to all the folks who helped quietly test Elm 0.19 over the past couple months. Your bug reports were extremely helpful. Some were quite fun to resolve! And thank you to the folks who updated packages and editors. Not everything is done, but we have a pretty solid start. Remember that people do free work because it is fun, not to get stressed by strangers. Based on my personal experiences and discussions with other authors, being patient online and supportive in person is the most productive path.

"""
