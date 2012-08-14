
import Data.List (intercalate)
import Website.Skeleton

section = text . bold . Text.height (5/4) . toText

content w =
  [ section "Source Code for this Website"
  , text $ toText "This website was written almost entirely in Elm, so if you want to see a larger example, you can browse the source code on " ++
           link "https://github.com/evancz/elm-lang.org" (toText "github") ++
           toText ". The download links are " ++
           link "https://github.com/evancz/elm-lang.org/downloads" (toText "here") ++
           toText ". The download also includes almost all of the examples on this site, which might " ++
           toText "be nice if you prefer to work on your own machine."
  , text $ toText "If you want to see the source code of a particular page in the online editor, " ++
           toText "just insert " ++ monospace (toText "edit/") ++ toText " after this sites domain name. For instance, " ++
           link "/edit/Examples.elm" (toText "elm-lang.org/" ++ bold (toText "edit/") ++ toText "Examples.elm") ++
           toText " is the source code for the examples page."
  ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
