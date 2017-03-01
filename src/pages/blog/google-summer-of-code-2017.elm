import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Blog
import Center



(=>) = (,)


main =
  Blog.blog
    "Elm + Google Summer of Code"
    ""
    Blog.evan
    (Blog.Date 2017 3 1)
    [ Center.markdown "600px" intro
    , div [Center.style "600px"]
        [div [ class "intrinsic-container" ]
          [ iframe
              [ src "https://www.youtube.com/embed/DSjbTC-hvqQ?start=845&end=1608&rel=0&autoplay=0"
              , attribute "allowfullscreen" ""
              ] []
          ]
        ]
    , Center.markdown "600px" part2
    ]



-- CONTENT


intro = """

Elm is participating in Google Summer of Code! **Undergraduate, masters, and PhD students have the chance to work on Elm over the summer with a stipend and mentorship.** I think this will be fun for students, but it will also be a great way to share knowledge and expertise within the Elm community and broader open source community.

The rest of this post is about what students and community members can do so that the Elm community gets the most out of this opportunity. Specifically, students can submit project proposals starting on 20 March, and **I think we will get the best proposals if experienced Elm community members help out!**


> For skimmers, more info is available at [the GSoC FAQ][faq], [the GSoC rules][rules], [Elm’s GSoC entry][org], [the project suggestion page][projects], [this wiki][wiki], and [the #gsoc channel on Slack][slack]. Please read this post if you want to help out though!

[faq]: https://developers.google.com/open-source/gsoc/faq
[rules]: https://summerofcode.withgoogle.com/rules/
[org]: https://summerofcode.withgoogle.com/organizations/6174333933191168/
[projects]: https://github.com/elm-lang/projects/blob/master/README.md
[wiki]: https://github.com/elm-lang/projects/wiki
[slack]: https://elmlang.slack.com/messages/gsoc/

## Ideal Projects

Participating in an open source community is more than just writing code. It is about building relationships and collaborating with people. **An ideal project will take your experience with Elm and channel it toward a concrete problem that interests you.** I highly recommend watching 12 minutes of the following talk to understand this better. The video will automatically start at the 14m6s mark, so just press play!

"""


part2 = """

When it comes to Google Summer of Code, we have a bunch of constraints that make it easier to pick out great projects:

  * Time is limited. Only about 12 weeks.
  * API design takes a couple years to learn, so it is not an ideal project when someone is new and time constrained.
  * Students may not finish everything, so the expected results should be valuable even if not everything is completed.

I have outlined [a bunch of good community projects](https://github.com/elm-lang/projects/blob/master/README.md), but I want to highlight a few that are ideal for GSoC.


### Package Search

The search feature of [package.elm-lang.org](http://package.elm-lang.org/) is quite rudimentary. Community members have already created “type search” [like this](http://klaftertief.github.io/elm-search/) which is really cool, but I think we would benefit from a more traditional search feature as well. I outline some ideas for this [right here](https://github.com/elm-lang/projects/blob/master/README.md#package-search).

In the end we’d want this service to live on its own server, so if it goes down, it does not take down the package website. It would also take in JSON and give out JSON, so the scope is limited to writing servers, which can be a great learning experience.

These natural constraints make it a great GSoC project. If the project is incomplete, nothing is blocked. In every case *the community* learns if full-text search is valuable, and if so, how much it costs. And perhaps the project will inspire collaborators or competitors!


### Exploratory Compiler and Optimization Work

There are a few projects that require a stronger background in compilers and programming languages. I think these would be great for older undergrads, masters students, and PhD students. The [project page](https://github.com/elm-lang/projects/blob/master/README.md) lists [exploring monomorphization](https://github.com/elm-lang/projects/blob/master/README.md#explore-monomorphizing-compilers) and [exploring WebAssembly](https://github.com/elm-lang/projects/blob/master/README.md#explore-webassembly). Both of these projects are primarily *exploratory*. In other words, a big part of the results would be a well-written literature review, so even if the technical artifacts are not perfect, they get things going.

I think there are a bunch of more sophisticated control flow analyses that could be interesting to look into. Maybe that means figuring out test coverage statically or trying to augment [elmjutsu](https://atom.io/packages/elmjutsu) to suggest fancier expressions based on types. So if you see:

```elm
longestName : List User -> Int
longestName users =
  users
    |> List.map .name
    |> ...
```

In the `...` we know we want to get from `List String` to `Int` so we can suggest functions like `List.length`. Perhaps we can rank suggestions based on which file or package they live in.

Point is, these are projects that push the Elm community in interesting directions, they have a large learning component for both the student and community, and you can achieve a great deal before much coordination is needed. For masters and PhD students, there are ways to fit projects like this into a broader research agenda, but we will have a strong preference for projects that (1) do not require language changes and (2) can reasonably expect a concrete result.

For folks with the background for this, I encourage you to talk to the creators of projects like elm-test, elm-format, elmjutsu, and sketch-n-sketch to learn what kind of analyses would be most useful in practice.


## Mentorship

I have created two collaborative resources for mentorship and support:

  - **The [#gsoc channel](https://elmlang.slack.com/messages/gsoc/) on the Elm slack.** If you want to help students refine projects, keep an eye on it! Once there are accepted projects, I want that channel to be a place for students, mentors, and knowledgeable community members to work together. This way we can quickly learn from the student’s project and they learn from our collective expertise.

  - **The [mentor wiki](https://github.com/elm-lang/projects/wiki).** Students will also have a specific mentor that will meet with them at least once a week. I think finding a match between interests, knowledge, and personalities is important, so please add your information if you are interested in being a mentor. Obviously not everyone can do it, but I think it makes sense to have a bunch of folks willing to help.


## Thanks and Good Luck

First, thank you to the folks who helped out with the Google Summer of Code application. I really appreciate your support!

Second, I am excited to see what kind of proposals we get. Hopefully this blog post helps you refine your ideas into something ambitious and practical!


"""
