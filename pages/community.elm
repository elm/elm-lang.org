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

For the latest posts, podcasts, and packages:

- [Twitter](https://twitter.com/elmlang)
- [Elm Weekly](https://www.elmweekly.nl/)

For more in-depth conversation:

* [Discourse](https://discourse.elm-lang.org/)
* [Slack](https://elmlang.herokuapp.com/)
* [Meetups](https://www.meetup.com/topics/elm-programming/all/)
* [Reddit](http://www.reddit.com/r/elm)

<br>


## Code of Conduct

Be kind, ask questions, and try to recognize [XY problems](https://en.wikipedia.org/wiki/XY_problem).

Read the full [Code of Conduct](https://github.com/elm-community/discussions/blob/master/code-of-conduct.md) as well!


## Share Code

* [Applications](https://ellie-app.com) &mdash; edit programs online and share them with others
* [Packages](http://package.elm-lang.org/) &mdash; learn about the packages you use


## Participate

The best way to get started in the community is to participate socially. Maybe that means setting up an [ElmBridge](https://github.com/elmbridge) event in your area or starting a meetup. Maybe it means helping friends, people at your company, or folks online.

If you already did some of that and want to get involved with code, please check out the following resources:

- [What is Success?](https://youtu.be/uGlzRt-FYto)
- [Code is the Easy Part](https://youtu.be/DSjbTC-hvqQ)
- [Building Trust](https://discourse.elm-lang.org/t/building-trust-what-has-worked/975)

We have primarily volunteer labor. One full-time dev, but outside of that, it is people working on nights and weekends because it is fun. This makes it hard to get to issues and pull requests as quickly as projects with paid "developer relations" staff to focus specifically on online interactions. So given our goals and resources, we have found that having good working relationships is crucial for collaboration on core code.

If you are a company evaluating Elm, I encourage you to DM if you are worried about something, and it is totally reasonable to circle back to Elm later!

"""
