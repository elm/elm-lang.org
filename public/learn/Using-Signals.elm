import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Using Signals"

main = skeleton "Learn" (content << min 600) <~ Window.dimensions


content w = width w [markdown|

# Using Signals

Coming soon!

|]


