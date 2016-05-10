module Message exposing (report)

import Html
import ColorScheme as C
import Markdown


report message =
  Markdown.toHtml [] message

