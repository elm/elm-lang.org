import Website.Skeleton (skeleton)
import Window

install = [markdown|

### Install

Use these [install instructions][1] to get Elm running on your machine.
You are getting:

1. **Compiler** &ndash; Turn Elm code into HTML, CSS, and JavaScript.

2. **Server** &ndash; Serve Elm files, images, videos, HTML, JavaScript,
   and anything else you can think of.

If you run into problems, you should email the [mailing list][2]
or report an issue to Elm's [source repository][3]

  [1]: https://github.com/evancz/Elm/blob/master/README.md#install "install instructions"
  [2]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "email list"
  [3]: https://github.com/evancz/Elm "source repository"

|]

other = [markdown|

### Build from Source

The [source code of the Elm compiler and server](https://github.com/evancz/Elm)
is available, so please browse, fork, and contribute to the project.

The [source code of this website](https://github.com/evancz/elm-lang.org) is also
available. Follow [these instructions][instruct] and you can easily get this entire website
running on your own machine and use Elm's interactive editor locally. From there
you can use it as a starting point for creating your own website!

 [instruct]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "install"

|]

info w =
  let hwidth = if w < 800 then w `div` 2 - 20 else 380 in
  flow right [ width hwidth install, spacer 40 10, width hwidth other ]

main = lift (skeleton info) Window.width
