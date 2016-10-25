-- Check out https://guide.elm-lang.org/core_language.html
-- for a guided tour of features like this!

import Html exposing (Html, ol, li, text)
import Html.Attributes exposing (style)
import String


{- TIP: Click on the String.reverse function below. You should see a link
to its documentation in top left corner of the editor. Now try String.left
and String.right!
-}


main =
  viewStrings
    [ "hello"
    , "ho" ++ "la"
    , String.reverse "desserts"
    , String.right 4 "foxglove"
    , String.left 20 multilineString
    ]


multilineString = """

All happy families are alike; each unhappy family is unhappy in its own way.

All was confusion in the Oblonskys’ house. The wife had found out that the
husband was having an affair with their former French governess, and had
announced to the husband that she could not live in the same house with him.
This situation had continued for three days now, and was painfully felt by the
couple themselves, as well as by all the members of the family and household.
They felt that there was no sense in their living together and that people who
meet accidentally at any inn have more connection with each other than they,
the members of the family and household of the Oblonskys. The wife would not
leave her rooms, the husband was away for the third day. The children were
running all over the house as if lost; the English governess quarreled with
the housekeeper and wrote a note to a friend, asking her to find her a new
place; the cook had already left the premises the day before, at dinner-time;
the kitchen-maid and coachman had given notice.

"""



-- HELPERS
-- These make things look prettier, but don't worry about them too much.
-- The stuff above about strings is the important part of this example!


viewStrings : List String -> Html msg
viewStrings strings =
  ol [style [("font-family","monospace")]] (List.map viewString strings)


viewString : String -> Html msg
viewString string =
  li [] [ text (toString string) ]
