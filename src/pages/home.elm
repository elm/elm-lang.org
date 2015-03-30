import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


main =
  div [] [ topBar, splash ]


(=>) = (,)


topBar =
  div [ class "navigation", id "tabs" ]
    [
      ul [] (List.map tab [ "examples", "docs", "community", "blog" ])
    ]


tab name =
  li []
    [ a [ class "tab", href ("/" ++ name) ] [ text name ]
    ]


splash =
  div [ class "navigation", id "splash" ]
    [ div [ size 100 20 ] [ text "elm" ]
    , div [ size 26 4 ] [ text "writing great code should be easy" ]
    , div [ size 16 4 ] [ text "the best of functional programming in your browser" ]
    , div [ size 26 30 ] [ a [ href "/try" ] [ text "install" ] ]
    ]


size height padding =
  style
    [ "font-size" => (toString height ++ "px")
    , "padding" => (toString padding ++ "px 0")
    ]