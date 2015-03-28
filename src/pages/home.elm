import Markdown
import Message exposing (report)

main = report <| Markdown.toElement """

# Elm

Temporary home page

  * [try](/try)
  * [examples](/examples)
  * [guide](/guide)
  * [community](/community)
  * [blog](/blog)
  * [packages](http://package.elm-lang.org/) &rarr;

"""
