
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


main : Program () () Never
main =
  Browser.document
    { init = \_ -> ((), Cmd.none)
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    , view = \_ ->
        { title = "Page not found"
        , body = [ notFound ]
        }
    }


notFound : Html msg
notFound =
  Markdown.toHtmlWith options
    [ style "width" "300px"
    , style "margin" "100px auto 0"
    , style "background" "#F5F5F5"
    , style "padding" "0 30px 10px"
    , style "border-top" "4px solid #1293D8"
    ]
    """

<h1><div>Poem 404
<div style="font-size:0.5em;font-weight:normal">Page Not Found</div></div>
</h1>

I shall be telling this with a sigh<br/>
Somewhere ages and ages hence:<br/>
Two roads diverged in a wood, and I&mdash;<br/>
I took the one less traveled by,<br/>
And that has made all the difference.

<p style="text-align:right;font-style:italic;">Robert Frost</p>

"""


options : Markdown.Options
options =
  { githubFlavored = Just { tables = False, breaks = False }
  , defaultHighlighting = Nothing
  , sanitize = False
  , smartypants = False
  }
