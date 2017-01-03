import Html exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton "community" [ Center.markdown "600px" community ]



community = """

# Community

  * [Community Packages](http://package.elm-lang.org/)
  * [Reddit][reddit]
  * [Slack][slack]
  * [Twitter][twitter]
  * [Real Life][real-life]
  * [Mailing list][list]
  * [Contributing](#contribute)

<br>


## Reddit

[reddit]: http://www.reddit.com/r/elm

[/r/elm][reddit] is a great place for friendly discussion! Common activities
include helping beginners, discussing blog posts, announcing and reviewing
packages, etc.

The online Elm community is all about learning and improvement, so even if you
think you know a lot, be humble and open to learning new things from anyone!

Also, be kind! Friendly disagreement is healthy, but [displacement][] of
personal anger onto friendly strangers is not.

[displacement]: https://en.wikipedia.org/wiki/Displacement_(psychology)


## Slack

[slack]: http://elmlang.herokuapp.com/

Got a quick question, but don't feel comfortable asking on Reddit?
Chatting on [Slack][slack] is a great way to quickly learn from a real person!

As for culture, prefer to ask rather than tell. You may be talking to someone
with no programming background or a PhD in programming languages, so to answer
a question well, you should start by asking some questions yourself! This way
we can avoid [XY problems][xy], and better yet, have a culture that is kind and
respectful to everyone.

[xy]: http://mywiki.wooledge.org/XyProblem


## Twitter

[twitter]: https://twitter.com/elmlang

Both [@elmlang][twitter] and [@czaplic](https://twitter.com/czaplic) tweet about
Elm a lot. The #elmlang hashtag is commonly used. Using #elm is okay, but people
tweet about weird stuff with that one sometimes.


## Real Life

There are meetups [all over the world][real-life] these days!

If you are thinking of starting a meetup in your city, check out how
[Evan](https://github.com/evancz/) and [Richard](https://github.com/rtfeldman) organize
the Elm user group in SF. A lot of existing meetups are structured based on
[what we learned from experience][hack-night].

[real-life]: https://www.elmlog.com/meetup_groups
[hack-night]: http://tech.noredink.com/post/142283641812/designing-meetups-to-build-better-communities


## Mailing list

[list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "elm-discuss"

The [elm-discuss][list] mailing list is the original online forum for Elm. We
used to do all discussion there, from beginner questions to language design
discussions. These days it is in between [/r/elm](https://www.reddit.com/r/elm/)
and [elm-dev](https://groups.google.com/d/msg/elm-dev/oZ3xW_nMPNo/0y8j-N8HCQAJ).
There are some people (and their expertise) only available through email!


## Contribute

I recommend watching [this talk](https://youtu.be/DSjbTC-hvqQ), which has a
big focus on growing Elm and collaborating.

Here are some of the ideas in words. **Every great contributor starts
gradually.** At first this means simply using Elm, maybe blogging about things
they find interesting. After that, some folks will find things they want to
make easier and build packages, like [elm-css][], that directly address their
personal needs. This can be a great learning experience if you get folks in
the community to review your API. It is also a great way to build relationships
and trust with people. From there, people may work on projects like
[these](https://github.com/elm-lang/projects) that are more core. This is
how [elm-format][] and [elm-test][] were created.

[elm-css]: https://github.com/rtfeldman/elm-css
[elm-format]: https://github.com/avh4/elm-format
[elm-test]: https://github.com/elm-community/elm-test/pull/24

Contribution usually means *technical* contribution, but community participation
is often more valuable! Helping people with questions. Writing blog posts. Creating
examples. This is all hugely positive for the community and the language, and I
think engineer minds tend to underestimate its importance. Hanging out in community
forums also means you have more data on what people are struggling with, making it
easier to find timely projects that match your skills and interests.

Point is, there are tons of ways to contribute! The goal should be to start gradually
and figure out what works as you build a relationship with the community.

"""
