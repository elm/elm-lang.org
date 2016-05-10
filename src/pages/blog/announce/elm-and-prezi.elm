import Blog
import Center


main =
  Blog.blog
    "Working on Elm full-time"
    "Elm ♥ Prezi"
    Blog.evan
    (Blog.Date 2013 5 21)
    [ Center.markdown "600px" content ]


content = """

I am now working on Elm full-time at [Prezi](http://prezi.com/)!

I have tried to anticipate the questions people might have,
[as has Péter Halácsy][prezi], but the
short version is that this is really amazing news for Elm!

  [prezi]: http://engineering.prezi.com/blog/2013/05/21/elm-at-prezi/

#### Q: Is Elm still free and open source?
Yes, of course! Elm is still totally open source under the BSD3 license
and all my work while at Prezi will be available forever under that license.
This is actually a stronger guarantee than before.

#### Q: Is this why [0.8][0.8] took so long?
Yes! I have been talking with Prezi for the past couple months.
I should be able to move a lot quicker now.

  [0.8]: /blog/announce/0.8

#### Q: What might change because of this?
I am still making the design decisions, so the goals are the same.
The biggest changes are that I will:

* Have time to really focus on Elm.
* Have the bandwidth to work more closely with contributors and set
  up [reasonable projects][projects] for people. Email [the list][list]
  if you are interested!
* Talk to Prezi designers and front-end developers to better
  understand their problems.

  [projects]: https://docs.google.com/spreadsheet/ccc?key=0AtAn2jvQYh8EdEZTSWtWWU9nSWkzYWlUajJjV0N0aHc#gid=0
  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

#### Q: Prezi is based in Budapest, so is Evan moving out there?
I will split my time 60/40 between San Francisco and Budapest.
Most Prezi engineers are in Budapest, but they have offices in both locations.

#### Q: What is in it for Prezi?
Prezi engineers love functional programming, but the tools for functional web
development are not very mature right now. Prezi hired me to make Elm ready
for production sooner rather than later, and allow them to use FRP to more
easily implement complex behavior.

#### Q: Is Prezi secretly trying to take control and make Elm all about zooming?
No :) Although I am a full-time employee of Prezi, I still have full control of the
direction of Elm, and my primary goal is the success of Elm as an independent project.

#### Q: How many people are working on Elm full-time?
Just me for now. Prezi is hiring for other roles though, so let me know
if you are interested. I know this probably wasn't a question you had, but
it is a lovely place to work nonetheless :P

"""
