import Website.Skeleton (skeleton)
import Window

instructions = [markdown|

# Install

Follow [these instructions][1] to get set up on your machine. You are getting:

1. *Compiler* &mdash; Turn Elm code into HTML, CSS, and JavaScript
2. *Server* &mdash; simple server that recompiles Elm files on refresh

  [1]: https://github.com/evancz/Elm/blob/master/README.md#install "install instructions"

#### Syntax Highlighting

There are some Elm specific highlighters.
Haskell syntax highlighting also tends to work pretty well.

* *Vim* &mdash; use Haskell mode or [try elm.vim](https://github.com/otavialabs/elm.vim)
* *Emacs* &mdash; turn on [haskell-mode](https://github.com/afeinberg/haskellmode-emacs#readme)
  for `.elm` files
* *Sublime Text* &mdash; [Elm Language Support](https://github.com/deadfoxygrandpa/Elm.tmLanguage)

#### Build from Source

[The Elm compiler and server](https://github.com/evancz/Elm) are on github.
[This website](https://github.com/evancz/elm-lang.org) is also
available with [setup instructions][instruct]. The server can
be the basis for your own website. It also lets you use
the interactive editor locally.

 [instruct]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "install"

#### Problem?

If you run into problems, you should email the [mailing list][2], ask
questions [on IRC](http://webchat.freenode.net/?channels=elm), or
report an issue to Elm's [source repository][3]

  [2]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "email list"
  [3]: https://github.com/evancz/Elm "source repository"

|]

info w =
  width w instructions

main = lift (skeleton info) Window.dimensions
