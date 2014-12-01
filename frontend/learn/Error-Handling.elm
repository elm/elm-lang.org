import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Error Handling"

main = skeleton "Learn" (content << min 600) <~ Window.dimensions


content w = width w [markdown|

# Error Handling

The [`Maybe`][maybe] library is one of the easiest ways to do error handling
in Elm. This page goes through some examples, building up to some strategies
that make error handling nice as things get more complicated.

[maybe]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Maybe

## A Function that may Fail

Say we would like to do 

|]
