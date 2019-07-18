import Html exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - Community"
    Skeleton.Community
    [ Center.markdown "600px" community ]



community = """

# Community

Folks tend to congregate in the following forums:

* [Slack](https://elmlang.herokuapp.com/)
* [Discourse](https://discourse.elm-lang.org/)
* [Reddit](http://www.reddit.com/r/elm)
* [Twitter](https://twitter.com/elmlang)
* [Meetups](https://www.meetup.com/topics/elm-programming/all/)

<br>


## Code of Conduct

Be kind, ask questions, and try to recognize [XY problems](https://en.wikipedia.org/wiki/XY_problem).

Read the full [Code of Conduct](https://github.com/elm-community/discussions/blob/master/code-of-conduct.md) as well!


## Share Code

* [Applications](https://ellie-app.com) &mdash; edit programs online and share them with others
* [Packages](http://package.elm-lang.org/) &mdash; learn about the packages you use


## Core Team

The following folks write command line tools, publish packages, run conferences, help newcomers, and everything between:

* [Evan Czaplicki](https://github.com/evancz/)
* [Richard Feldman](https://github.com/rtfeldman/)
* [Robin Heggelund Hansen](https://github.com/Skinney)
* [Aaron VonderHaar](https://github.com/avh4)
* [Brian Hicks](https://github.com/brianhicks/)
* [Luke Westby](https://github.com/lukewestby)
* [Ilias Van Peer](https://github.com/zwilias/)
* [Noah](https://github.com/eeue56)


## Participate

The best way to get started in the community is to participate socially. Maybe that means setting up an [ElmBridge](https://github.com/elmbridge) event in your area or starting a meetup. Maybe it means helping friends, people at your company, or folks online.

If you want to get involved with code, please check out the following resources:

- [What is Success?](https://youtu.be/uGlzRt-FYto)
- [Code is the Easy Part](https://youtu.be/DSjbTC-hvqQ)
- [Building Trust](https://discourse.elm-lang.org/t/building-trust-what-has-worked/975)

We have primarily volunteer labor, and having good working relationships is an important aspect of collaboration like this.

"""
