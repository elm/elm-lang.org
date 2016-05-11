import Html exposing (li, text, ul)
import Html.Attributes exposing (class)


{-| This snippet uses the <ul> and <li> tags to create an unordered
list of French grocery items. Notice that all occurrences of 'ul'
and 'li' are followed by two lists. The first list is for any HTML
attributes, and the second list is all the HTML nodes inside the
tag.

Et maintenant le voyage au supermarché!
-}
main =
  ul [class "grocery-list"]
    [ li [] [text "Pamplemousse"]
    , li [] [text "Ananas"]
    , li [] [text "Jus d'orange"]
    , li [] [text "Boeuf"]
    , li [] [text "Soupe du jour"]
    , li [] [text "Camembert"]
    , li [] [text "Jacques Cousteau"]
    , li [] [text "Baguette"]
    ]


-- Thanks to "Flight of the Conchords" for creating "Foux Du Fafa"
