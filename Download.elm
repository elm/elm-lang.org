
import Website.Skeleton

section = text . bold . Text.height (7/6) . toText

info w = flow down . map (width w) . addSpaces $
  [ section "Installation"
  , text $ toText "See these " ++ link "https://github.com/evancz/Elm/blob/master/README.md" (toText "install instructions") ++
           toText " to get Elm running on your machine. If you run into problems, you should email the " ++
           link "https://groups.google.com/forum/?fromgroups#!forum/elm-discuss" (toText "mailing list") ++
           toText " or report an issue to Elm's " ++
           link "https://github.com/evancz/Elm" (toText "source repository") ++ toText "."
  , rectangle 1 1
  , section "Thesis on Elm"
  , text $ toText "My " ++
           link "http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf" (toText "thesis on Elm") ++
           toText " is available though. It provides a more formal definition of Elm and a discription of Concurrent FRP, a new " ++
           toText "approach to efficient Functional Reactive Programming."
  ]
 
main = lift (skeleton info) Window.width
