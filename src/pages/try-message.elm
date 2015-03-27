import Markdown
import Message exposing (report)

main = report <| Markdown.toElement """

# Elm 0.15 alpha

Check out the change logs:

  * <a href="/blog/draft-announcement" target="_top">Very rough draft announcement</a>
  * <a href="https://github.com/elm-lang/elm-compiler/blob/master/changelog.md" target="_top">
    Changes to compiler / syntax</a>
  * <a href="https://github.com/elm-lang/core/blob/master/changelog.md" target="_top">
    Changes to the core libraries</a>

Here are some example programs:

  * <a href="/examples/zip-codes" target="_top">Zip Code Lookup</a>
  * <a href="/examples/flickr" target="_top">Flickr Search</a>

"""
