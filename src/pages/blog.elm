import Html exposing (..)

import Center
import TopBar


main =
  div []
    [ TopBar.topBar "blog"
    , Center.markdown "600px" blog
    ]


blog = """


# Blog

### Articles

 * [Introducing Elm Reactor](/blog/introducing-elm-reactor)
 * [Blazing Fast HTML](/blog/blazing-fast-html)
 * [Elm&rsquo;s Time Traveling Debugger](http://debug.elm-lang.org)
 * [Elm package manager](/blog/announce/PackageManager.elm)
 * [Elm REPL](/blog/announce/Repl.elm)
 * [Hot-swapping in Elm](/blog/interactive-programming)
 * [Concepts behind the Elm Logo](https://prezi.com/npjjrmt_badc/tangrams-logo/)
 * [Elm in VentureBeat](http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/)
 * [Elm &hearts; Prezi](/blog/announce/Elm-and-Prezi.elm)
 * [Escape from Callback Hell](/blog/escape-from-callback-hell)
 * [Making Pong](/blog/making-pong)

### Release Notes

* <code>[0.15][15]        &nbsp; &nbsp; Apr &nbsp; &nbsp; &nbsp; &nbsp;</code>Tasks, better HTTP library
* <code>[0.14.1][141]            &nbsp; Jan 2015 &nbsp; </code>HTML through main
* <code>[0.14][14]        &nbsp; &nbsp; Dec &nbsp; &nbsp; &nbsp; &nbsp;</code>Package manager, parallel builds, JSON
* <code>[0.13][13]        &nbsp; &nbsp; Sep &nbsp; &nbsp; &nbsp; &nbsp;</code>Elm Reactor
* <code>[0.12.3][123]            &nbsp; May &nbsp; &nbsp; &nbsp; &nbsp;</code>3D rendering with WebGL
* <code>[0.12.1][121]            &nbsp; May &nbsp; &nbsp; &nbsp; &nbsp;</code>Fast Immutable Arrays
* <code>[0.12][12]        &nbsp; &nbsp; Mar &nbsp; &nbsp; &nbsp; &nbsp;</code>Interactive UI Elements that are easy and pure
* <code>[0.11][11]        &nbsp; &nbsp; Jan 2014 &nbsp; </code>Ports: drastically improved FFI
* <code>[0.10.1][101]            &nbsp; Dec &nbsp; &nbsp; &nbsp; &nbsp;</code>elm-get integration
* <code>[0.10][10]        &nbsp; &nbsp; Oct &nbsp; &nbsp; &nbsp; &nbsp;</code>Strings, Colors, Operators
* <code>[0.9][9]    &nbsp;&nbsp; &nbsp; Aug &nbsp; &nbsp; &nbsp; &nbsp;</code>Fast and reliable type inference
* <code>[0.8][8]    &nbsp;&nbsp; &nbsp; May &nbsp; &nbsp; &nbsp; &nbsp;</code>HTML/JS integration
* <code>[0.7.1][71]        &nbsp;&nbsp; Feb &nbsp; &nbsp; &nbsp; &nbsp;</code>Touch, Either, Keyboard
* <code>[0.7][7]    &nbsp;&nbsp; &nbsp; Jan 2013 &nbsp; </code>Extensible records
* <code>[0.6][6]    &nbsp;&nbsp; &nbsp; Dec &nbsp; &nbsp; &nbsp; &nbsp;</code>Whitespace sensitivity
* <code>[0.5][5]    &nbsp;&nbsp; &nbsp; Oct &nbsp; &nbsp; &nbsp; &nbsp;</code>Dictionaries, Sets, and Automata
* <code>[0.4][4]    &nbsp;&nbsp; &nbsp; Sep &nbsp; &nbsp; &nbsp; &nbsp;</code>Markdown
* <code>0.3.6              &nbsp;&nbsp; Aug &nbsp; &nbsp; &nbsp; &nbsp;</code>JSON support
* <code>0.3.5              &nbsp;&nbsp; Jun &nbsp; &nbsp; &nbsp; &nbsp;</code>JavaScript FFI
* <code>0.3         &nbsp;&nbsp; &nbsp; Jun &nbsp; &nbsp; &nbsp; &nbsp;</code>Modules
* <code> 0.1        &nbsp;&nbsp; &nbsp; Apr 2012 &nbsp; </code>Initial Release

  [3]:  http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/
  [4]:  /blog/release/0.4.0
  [5]:  /blog/release/0.5.0
  [6]:  /blog/release/0.6
  [7]:  /blog/release/0.7
  [71]: /blog/release/0.7.1
  [8]:  /blog/release/0.8
  [9]:  /blog/release/0.9
  [10]: /blog/release/0.10
  [101]: /blog/release/0.10.1
  [11]: /blog/release/0.11
  [12]: /blog/release/0.12
  [121]: /blog/release/0.12.1
  [123]: /blog/release/0.12.3
  [13]: /blog/release/0.13
  [14]: /blog/release/0.14
  [141]: https://groups.google.com/forum/#!topic/elm-announce/6zRwjN68Ap0
  [15]: /blog/release/0.15

### Conference Videos

* [StrangeLoop 2013][sl2013] &mdash; The best one so far! Teaches the basics of
  graphics and FRP in Elm and builds up to implementing a basic [Mario
  game](/edit/examples/Intermediate/Mario.elm) *and* it is pretty fun to watch.
* [mloc.js 2013][mlocjs] &mdash; a less polished version of the StrangeLoop 2013 talk.
* [StrangeLoop 2012][sl2012] &mdash; First conference talk ever. Covers the basics of
  graphics and FRP before Elm even had its record system!

 [sl2013]: http://www.infoq.com/presentations/elm-reactive-programming
 [mlocjs]: http://www.ustream.tv/recorded/29330499
 [sl2012]: http://www.infoq.com/presentations/Elm

"""