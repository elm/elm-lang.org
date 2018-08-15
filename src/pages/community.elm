import Html exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - Community"
    "community"
    [ Center.markdown "600px" community ]



community = """

# Community

* [Discourse](https://discourse.elm-lang.org/) &mdash; ask questions, get
feedback, and show folks what you're working on.
* [Reddit](http://www.reddit.com/r/elm) &mdash; discuss blog posts.
* [Slack](http://elmlang.herokuapp.com/) &mdash; ask quick questions, ask advice, help people out, talk through ideas, etc.
* [Twitter](https://twitter.com/elmlang) &mdash; share interesting Elm stuff.
* [Meetups](https://www.elmlog.com/meetup_groups) &mdash; meet other Elm folks in real life, [make something](https://blog.noredink.com/post/142283641812/designing-meetups-to-build-better-communities)!

Be kind! Ask questions! Try to recognize [XY problems](https://en.wikipedia.org/wiki/XY_problem)! We wrote up more advice in the [Code of Conduct](https://github.com/elm-community/discussions/blob/master/code-of-conduct.md).

<br>

# Code

* [Community Packages](http://package.elm-lang.org/) &mdash; package ecosystem with [these values](https://youtu.be/uGlzRt-FYto).
* [Ellie](https://ellie-app.com) &mdash; online editor for easily sharing Elm programs.

<br>

# Core Team

The following folks write command line tools, publish packages, run conferences, help newcomers, and everything between:

* [Evan Czaplicki](https://github.com/evancz/)
* [Richard Feldman](https://github.com/rtfeldman/)
* [Aaron VonderHaar](https://github.com/avh4)
* [Brian Hicks](https://github.com/brianhicks/)
* [Luke Westby](https://github.com/lukewestby)
* [Ilias Van Peer](https://github.com/zwilias/)
* [Noah](https://github.com/eeue56)

<br>

# Participate

The following resources outline what it means to participate in the Elm community:

- [What is Success?](https://youtu.be/uGlzRt-FYto)
- [Code is the Easy Part](https://youtu.be/DSjbTC-hvqQ)
- [Building Trust](https://discourse.elm-lang.org/t/building-trust-what-has-worked/975)

Please go through them! Especially if you are interested in working on  packages or tools. It is not about code. It is not about more features and more stars. These resources describe what it _is_ about in Elm, and they will likely save you a great deal of time!

"""
