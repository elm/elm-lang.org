import Markdown
import Website.Message exposing (report)

main = report <| Markdown.toElement """

# Elm 0.15 alpha

Check out the change logs:

  * <a href="/Draft-Announcement.elm" target="_top">Very rough draft announcement</a>
  * <a href="https://github.com/elm-lang/elm-compiler/blob/master/changelog.md" target="_top">
    Changes to compiler / syntax</a>
  * <a href="https://github.com/elm-lang/core/blob/master/changelog.md" target="_top">
    Changes to the core libraries</a>

Here are some example programs:

  * <a href="/edit/examples/ZipCodes.elm" target="_top">Zip Code Lookup</a>
  * <a href="/edit/examples/Flickr.elm" target="_top">Flickr Search</a>

"""
