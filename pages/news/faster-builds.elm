import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Skeleton
import Center


main =
  Skeleton.news
    "Road to Elm 1.0"
    "Faster Builds with Elm 0.19.2"
    Skeleton.evan
    (Skeleton.Date 2026 7 6)
    [ Center.markdown "600px" content
    ]


content : String
content = """

# Road to Elm 1.0

I have a lot of compiler improvements piled up, and the road to 1.0 is essentially to incorporate them into Elm in a sequence of small releases. The release today is a little compiler performance improvement to just get things started, and I hope you will give it a try!

> **Note:** This is a patch release, so existing users should be able to just download [the new version](https://github.com/elm/compiler/releases/tag/0.19.2) and start using it with existing projects.


## Faster Builds with Elm 0.19.2

I love a fast compiler. So many powerful corporations are competing to waste my time, and if you give them a second, they will show you some statistically optimal “content” and try to take as much time as they can. So I like my incremental compiles to take <400ms so there is no time to switch away to something else.

So I was very happy to see that Elm 0.19.2 compiles 850k lines of Elm code from scratch in 5.7 seconds, and then takes <350ms for incremental builds. Nice! My compiler can help me stay focused and happy with my work!

The actual improvements in 0.19.2 are focused on allocating a little bit less during parsing. For the 850k lines I was experimenting with, I saw:

* 20% lower copying in GC
* 10% lower peak memory usage
* 7% faster overall

In other words, spending less time in GC makes things faster! Who would have guessed?! So for programs over 500k lines, you may see a modest improvement. For smaller programs, you will just see that the Elm compiler continues to be very fast!

In real world testing, results ranged from slight improvements to a 1.9x improvement when compiling 351 modules: from 4.981 seconds to 2.595 seconds for compiling from scratch. I am curious to hear if those of you with larger projects will notice any change!


## What is Next?

As some of you may know, I have been working on a database-related compiler called [Acadia](https://acadia.engineering) for a while. That work went really well! It is currently in private alpha, and I am excited to share it publicly later this year. In doing all that work, I figured out a bunch of compiler/language ideas that are useful in Elm as well. Some are simple performance things (like the faster parser) and others are more visible features like `equatable` and `hashable` types. To get these ideas integrated into Elm in a nice way, it makes sense to do a series of small non-breaking releases, and then go 1.0 for the final touches.
"""
