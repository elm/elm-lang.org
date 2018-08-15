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

Folks tend to congregate in the following forums:

* [Slack](http://elmlang.herokuapp.com/) &mdash; ask quick questions, help others, talk through ideas
* [Discourse](https://discourse.elm-lang.org/) &mdash; ask questions, get feedback, show off your work
* [Reddit](http://www.reddit.com/r/elm) &mdash; discuss blog posts
* [Twitter](https://twitter.com/elmlang) &mdash; hear what is going on, share your experiences
* [Meetups](https://www.elmlog.com/meetup_groups) &mdash; meet in real life, make something together

<br>

## Code of Conduct

Be kind, ask questions, and try to recognize [XY problems](https://en.wikipedia.org/wiki/XY_problem).

Read the full [Code of Conduct](https://github.com/elm-community/discussions/blob/master/code-of-conduct.md) as well!

<br>

## Share Code

* [Applications](https://ellie-app.com) &mdash; edit programs online and share them with others
* [Packages](http://package.elm-lang.org/) &mdash; learn about the packages you use

<br>

## Core Team

The following folks write command line tools, publish packages, run conferences, help newcomers, and everything between:

* [Evan Czaplicki](https://github.com/evancz/)
* [Richard Feldman](https://github.com/rtfeldman/)
* [Aaron VonderHaar](https://github.com/avh4)
* [Brian Hicks](https://github.com/brianhicks/)
* [Luke Westby](https://github.com/lukewestby)
* [Ilias Van Peer](https://github.com/zwilias/)
* [Noah](https://github.com/eeue56)

<br>

## Participate

The following resources outline what it means to participate in the Elm community:

- [What is Success?](https://youtu.be/uGlzRt-FYto)
- [Code is the Easy Part](https://youtu.be/DSjbTC-hvqQ)
- [Building Trust](https://discourse.elm-lang.org/t/building-trust-what-has-worked/975)

Please go through them! Especially if you are interested in working on  packages or tools. It is not about code. It is not about more features and more stars. These resources describe what it _is_ about in Elm, and they will likely save you a great deal of time!

"""
