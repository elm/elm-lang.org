import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


main =
  Blog.blog
    "New Adventures for Elm"
    "Joining NoRedInk and creating Elm Software Foundation"
    Blog.evan
    (Blog.Date 2016 1 4)
    [ Center.markdown "600px" content
    ]



(=>) = (,)


content = """

2015 was quite an exciting year for Elm. It is now in use at NoRedInk,
CircuitHub, Prezi, Gizra, Beautiful Destinations, Your CRM Link, TruQu, and
more. Website traffic tripled, and folks are creating tons of great
[projects](http://builtwithelm.co/) and
[packages](http://package.elm-lang.org/). **It took 45 years, but ML-style
typed functional programming is finally catching on!** So 2015 was pretty
great, but we already have some exciting news for 2016.


## Joining NoRedInk

Today is my first day as an Open Source Engineer at NoRedInk, where I will
continue my work on Elm! (See [Marcos’s post][nri] for more details.)

[nri]: http://tech.noredink.com/post/136615783598/welcome-evan

Why is this exciting? Well, you may know their lead frontend engineer ([Richard
Feldman](https://twitter.com/rtfeldman)) from his excellent
[talks](https://youtu.be/6EdXaWfoslc) and
[projects](https://youtu.be/BfzjuhX4wJ0?t=9h16m48s), but I have had the
privilege of learning from him personally over the past year. Our discussions
created quite a powerful feedback loop. He inspired
[start-app](https://github.com/evancz/start-app), helped crystallize some core
communication [ideas](https://youtu.be/oYk8CKH7OhE), and motivated my work on
[error messages](/blog/compilers-as-assistants) with his
infectious excitement. Meanwhile Richard was using Elm at work more and more.
Today NoRedInk has about 5 engineers writing Elm full-time ([and is looking for
more](https://www.noredink.com/jobs)) and all of their new frontend code is
being written in Elm.

Part of what makes this work really well is that none of us are religious about
types or functional programming. We want to make a great product, and it just
so happens that Elm and NoRedInk both get better when we focus on that. The
whole NoRedInk team is extremely talented and fun to work with, so I am excited
to work closely with such great folks!


## Creating Elm Software Foundation

Even though I am changing jobs, Prezi is continuing to support Elm. [Prezi hired
me to work on Elm](/blog/announce/elm-and-prezi) more than two years ago. Their
CTO, Péter Halácsy, knew that (a) his engineers would get more done in an
ML-family language and (b) the big players in JS weren’t going to realize that
on their own any time soon. So he decided to support Elm in 2013, when
immutability was still only for crazy people and distributed systems. Since
then Elm has grown into a solid set of tools and great community, supporting a
bunch of companies, tens of thousands of programmers, and millions of end
users. I am already forever grateful that Prezi took a chance on Elm, but this
is not where the story ends.

In addition to joining NoRedInk, I am establishing the non-profit Elm Software
Foundation and Prezi is making a generous donation to start things off. At
first, the foundation will help with things like “Elm Summer of Code”, meetup
and conference costs, and the cost of keeping all the Elm servers running. We
will see where it goes from there! For now, it cannot handle community
donations, but we’ll work on that.


## Thank you

Finally, I just want to say thank you to everyone who has used and supported
the project so far! Whether it is creating packages, answering questions,
setting up meetups, working on hobby projects, or even just excitedly telling a
friend about the error messages, the friendly and thoughtful community is a
huge part of what makes Elm fun. I am excited to see what we do in 2016!

"""
