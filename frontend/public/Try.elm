import Markdown
import Website.Message exposing (report)

main = report <| Markdown.toElement """

# Elm 0.15 alpha

[Check out the change log][changes]! Syntax for imports and ports
is a bit different and there were some API changes in core.

  [changes]: https://github.com/elm-lang/core/blob/master/changelog.md

Here are some example programs that demonstrate Promises:

  * Http -
    <a href="/edit/examples/" target="_top">example</a>
    / <a href="https://github.com/evancz/" target="_top">API</a>
  * Console -
    <a href="/edit/examples/" target="_top">example</a>
    / <a href="https://github.com/evancz/" target="_top">API</a>
  * Local Storage -
    <a href="/edit/examples/" target="_top">example</a>
    / <a href="https://github.com/evancz/" target="_top">API</a>

"""
