import Html exposing (..)

import Skeleton
import Center


main =
  Skeleton.news
    "Elm + Bekk"
    "Towards a Homegrown Programming Language"
    Skeleton.evan
    (Skeleton.Date 2019 9 3)
    [ Center.markdown "600px" content ]


content = """

Did you know that Elm is big in Norway?

So much so that a Norwegian tech firm (Bekk) is supporting a core team member (Robin) to spend one workday each week working on Elm!

You can learn a lot about a programming language from who pays for it, so I will try to explain why this is important for Elm.


# Who is Robin?

Robin got involved with the core team after doing some extremely thorough work on data structures, like [making Dict.insert 171% faster](https://groups.google.com/d/msg/elm-dev/--fK-wMoDig/p6zF4-5sAgAJ). I was always excited when I get a document from Robin because I knew I would be learning about a bunch of performance experiments, along with nicely organized profiling information. Over time, a lot of this work has made it into elm/core and the ecosystem has quietly gotten faster and more reliable as a result!

And that was all while working on personal time! It has been such a joy to work with Robin, and it makes me really happy to see Bekk investing in his work on Elm!


# Why is Bekk doing this?

Bekk is one of the larger design and tech companies in Norway, and they are using Elm on some of their most important projects. For example, helping people applying for drivers licence with the National Public Road Administration and selling railway tickets from Vy, Norway's largest transport provider. They particularly like how quickly interns can learn Elm and become productive!

So Bekk benefits from faster data structures like everyone else, but investing in core team members is not just about the ecosystem benefits. Companies that make serious ongoing investments in the core team members become a hub for the community. It is fun to be around that kind of work, and it feels good to work at a company that is supporting a community you appreciate!

Hvis du kan forstå norsk, besøk gjerne [bloggen](https://blogg.bekk.no/) deres!


# Why is this important for Elm?

Many languages are funded as strategic plays for [Traffic Acquisition Costs](https://fourweekmba.com/traffic-acquisition-cost/), cloud service moats, app store revenue, etc. Getting caught up in the power plays of big name companies has a huge impact on how a language and community develops. E.g. languages tend to have features to cover a wide range of preferences, online interactions tend to have more of a "customer service" vibe, you get some consumer tech firms applying the "iterate fast, see what works" mentality to programming frameworks, etc. Those are obvious benefits for many people, but not for everyone.

I am hoping Elm will follow a path similar to Python. [Their path](https://python-history.blogspot.com/2009/01/personal-history-part-1-cwi.html) started with small companies giving the language a chance. Those companies got larger over time, and the core team ended up distributed between a bunch of companies. That means the core team was hearing perspectives from more companies, more industries, and more people. It seems to me that when a community grows by investing in itself, by building personal relationships, by sharing their experiences word of mouth, you end up with something much more than a programming language!

So in an industry obsessed with making things faster and cheaper without regard for the social or structural implications, it makes me really happy that Bekk is investing in the homegrown path for Elm. Thank you!

"""
