import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - Examples"
    "examples"
    [ Center.markdown "600px" content
    , div [ class "buttons", style "text-align" "center" ]
        [ a [ href "https://guide.elm-lang.org/" ] [ text "Official Guide" ]
    ]


content = """

# Examples

Elm was originally released with a simple online editor and a small set of
examples. Not all of the examples match nicely with the modern learning resources
about Elm, so it felt like it would be better to point folks to resources like
[The Official Guide](https://guide.elm-lang.org/) for now.

More specifically, the initial vision for the online editor and examples was to
make it really easy for beginners to start playing around. It had simple 2D
games and 3D animations. It was really neat, but over time, the learning resources
become more focused on folks trying to make web apps. Many examples did not really
fit with that, making the examples feel out of place for the audience coming from
JavaScript.

Both of these audiences are important though! So rather than just keeping it
sort of confusing for everyone, I think it makes sense to start improving the
learning resources for beginners. In the end, someone new to programming and
someone coming from JavaScript are going to be interested in very different
examples, so focusing on different audiences directly seems like the best path
to figuring out how an examples page should work. Especially when there are all
sorts of different people checking out the language these days.

"""
