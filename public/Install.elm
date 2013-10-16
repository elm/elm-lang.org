import Website.Skeleton (skeleton)
import Window

instructions = [markdown|

# Install

Once you outgrow [the online editor](/try), follow [these instructions][1]
to get Elm set up on your machine. You are getting:

1. *Compiler* &mdash; turn Elm code into HTML, CSS, and JavaScript
2. *Server* &mdash; serve a project, recompile Elm files on refresh

  [1]: https://github.com/evancz/Elm/blob/master/README.md#install "install instructions"

#### Syntax Highlighting

If you cannot find an Elm-specific highlighter, Haskell syntax highlighting tends
to work pretty well.

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

## Release Notes

* <code>[0.10][10]        &nbsp;&nbsp; Oct &nbsp; &nbsp; &nbsp; </code>Native Strings
* <code>[0.9][9]    &nbsp;&nbsp;&nbsp; Aug &nbsp; &nbsp; &nbsp; </code>Fast and reliable type inference
* <code>[0.8][8]    &nbsp;&nbsp;&nbsp; May &nbsp; &nbsp; &nbsp; </code>HTML/JS integration
* <code>[0.7.1][71]             &nbsp; Feb &nbsp; &nbsp; &nbsp; </code>Touch, Either, Keyboard
* <code>[0.7][7]    &nbsp;&nbsp;&nbsp; Jan 2013&nbsp; </code>Extensible records
* <code>[0.6][6]    &nbsp;&nbsp;&nbsp; Dec &nbsp; &nbsp; &nbsp; </code>Whitespace sensitivity
* <code>[0.5][5]    &nbsp;&nbsp;&nbsp; Oct &nbsp; &nbsp; &nbsp; </code>Dictionaries, Sets, and Automata
* <code>[0.4][4]    &nbsp;&nbsp;&nbsp; Sep &nbsp; &nbsp; &nbsp; </code>Markdown
* <code>[0.3.6][36]             &nbsp; Aug &nbsp; &nbsp; &nbsp; </code>JSON support
* <code>[0.3.5][35]             &nbsp; Jun &nbsp; &nbsp; &nbsp; </code>JavaScript FFI
* <code>[0.3][3]    &nbsp;&nbsp;&nbsp; Jun &nbsp; &nbsp; &nbsp; </code>Modules
* <code> 0.1        &nbsp;&nbsp;&nbsp; Apr 2012&nbsp; </code>Initial Release

  [3]:  http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Modules"
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Integration"
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/ "JSON"
  [4]:  /blog/announce/0.4.0.elm "Graphics Upgrade"
  [5]:  /blog/announce/0.5.0.elm "Libraries"
  [6]:  /blog/announce/0.6.elm "Time, Date, and Syntax"
  [7]:  /blog/announce/0.7.elm "Extensible Records & More"
  [71]: /blog/announce/0.7.1.elm "Touch, Keyboard, Either, etc."
  [8]:  /blog/announce/0.8.elm "HTML/JS integration"
  [9]:  /blog/announce/0.9.elm "type inference"
  [10]: /blog/announce/0.10.elm "native strings"

|]

info w =
  width w instructions

main = lift (skeleton info) Window.dimensions
