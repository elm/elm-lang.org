
button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (240/255) (241/255) (244/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , body outer inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

----------------------

section = text . bold . Text.height (5/4) . toText

info w = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map ((\e x -> e) . width w) $
  [ section "Download and Install Elm"
  , text $ toText "The source code is available at " ++ link "https://github.com/evancz/Elm" (toText "github") ++
           toText ", and the compiler/dev-server is available through " ++
           link "http://hackage.haskell.org/package/Elm" (toText "Hackage") ++
           toText ". See these " ++ link "https://github.com/evancz/Elm/blob/master/README.md" (toText "install instructions") ++
           toText " to get Elm running on your machine."
  , text $ toText "If you have questions, please take advantage of " ++
           link "http://www.reddit.com/r/haskell/comments/subvk/elm_source_code_and_compilerserver_now_available/" (toText "this reddit post") ++
           toText " or the " ++ link "https://groups.google.com/forum/?fromgroups#!forum/elm-discuss" (toText "mailing list") ++ toText "."
  , plainText "&nbsp;"
  , section "Download Elm Thesis"
  , text $ toText "My recently completed " ++
           link "http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf" (toText "thesis on Elm") ++
           toText " is available though. It provides a more formal definition of Elm and a discription of Concurrent FRP, a new " ++
           toText "approach to efficient Functional Reactive Programming."
  ]
 
body outer inner = width outer . box 2 . flow down . (:) (plainText "&nbsp;") $ info inner

main = lift (skeleton body) Window.width
