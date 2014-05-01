import Website.Skeleton (skeleton, installButtons)
import Window
import Graphics.Input as Input

main = lift (skeleton info) Window.dimensions

info w = flow down
  [ spacer w 20
  , installButtons w
  , width w instructions ]


instructions = [markdown|

To build from source or [contribute](/Contribute.elm), fork the
compiler/server [on github](https://github.com/elm-lang/Elm).
This website is also [available](https://github.com/elm-lang/elm-lang.org)
with [setup instructions][instruct]. It can be the basis for your own website
or let you use the interactive editor locally.
Use [`elm-repl`](https://github.com/elm-lang/elm-repl)
to interact with specific functions in a multi-module project.

 [instruct]: https://github.com/elm-lang/elm-lang.org#template-for-creating-elm-websites "install"

If you run into problems, email the [mailing list][1], ask
questions [on IRC](http://webchat.freenode.net/?channels=elm), or
report an issue to Elm's [source repository][2].

  [1]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "email list"
  [2]: https://github.com/elm-lang/Elm "source repository"

#### Syntax Highlighting

* *Sublime Text* &mdash; [Elm Language Support](https://github.com/deadfoxygrandpa/Elm.tmLanguage)
* *Emacs* &mdash; turn on [haskell-mode](https://github.com/afeinberg/haskellmode-emacs#readme) for `.elm` files
* *Vim* &mdash; use Haskell mode or try [elm.vim](https://github.com/otavialabs/elm.vim)
* *Other* &mdash; Haskell syntax highlighting tends to work pretty well

#### Release Notes

* <code>[0.12.1][121]            &nbsp;May &nbsp; &nbsp; &nbsp; </code>Fast Immutable Arrays
* <code>[0.12][12]        &nbsp;&nbsp; Mar &nbsp; &nbsp; &nbsp; </code>Interactive UI Elements that are easy and pure
* <code>[0.11][11]        &nbsp;&nbsp; Jan 2014&nbsp; </code>Ports: drastically improved FFI
* <code>[0.10.1][101]            &nbsp;Dec &nbsp; &nbsp; &nbsp; </code>`elm-get` integration
* <code>[0.10][10]        &nbsp;&nbsp; Oct &nbsp; &nbsp; &nbsp; </code>Strings, Colors, Operators
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
  [101]: /blog/announce/0.10.1.elm "elm-get integration"
  [11]: /blog/announce/0.11.elm "ports"
  [12]: /blog/announce/0.12.elm "user input"
  [121]: /blog/announce/0.12.1.elm "Fast Immutable Arrays"

|]
