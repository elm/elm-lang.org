import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Using Signals"

main = skeleton "Learn" (content << min 600) <~ Window.dimensions


content w = width w [markdown|

# Using Signals

Signals are values that change over time. You can learn more about the basics
of signals in [this post][frp] and in [the examples](/Examples.elm). This post
focuses on common design patterns that you will definitely see when programming
with signals in Elm.

[frp]: /learn/What-is-FRP.elm

To be continued!

|]


