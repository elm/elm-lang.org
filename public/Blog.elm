import Website.Widgets (bigLogo, installButtons)
import Website.Skeleton (skeleton)
import Website.BigTiles as Tile
import Website.ColorScheme as C

import Text
import Window

port title : String
port title = "Blog"

main = skeleton "Blog" content <~ Window.dimensions

content outer =
    let center elem =
            container outer (heightOf elem) middle elem
    in  center (width (min 600 outer) blog)


blog = [markdown|

# Blog

### Articles

 * [Introducing Elm Reactor](/blog/Introducing-Elm-Reactor.elm)
 * [Blazing Fast HTML](/blog/Blazing-Fast-Html.elm)
 * [Elm&rsquo;s Time Traveling Debugger](http://debug.elm-lang.org)
 * [Elm package manager](/blog/announce/PackageManager.elm)
 * [Elm REPL](/blog/announce/Repl.elm)
 * [Hot-swapping in Elm](/blog/Interactive-Programming.elm)
 * [Concepts behind the Elm Logo](https://prezi.com/npjjrmt_badc/tangrams-logo/)
 * [Elm in VentureBeat](http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/)
 * [Elm &hearts; Prezi](/blog/announce/Elm-and-Prezi.elm)
 * [Escape from Callback Hell](/learn/Escape-from-Callback-Hell.elm)
 * [Making Pong](/blog/Pong.elm)

### Release Notes

* <code>[0.13][13]        &nbsp;&nbsp; Sep &nbsp; &nbsp; &nbsp; </code>Architecture improvements and Elm Reactor integration
* <code>[0.12.3][123]            &nbsp;May &nbsp; &nbsp; &nbsp; </code>3D rendering with WebGL
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
  [123]: /blog/announce/0.12.3.elm "3D rendering with WebGL"
  [13]: /blog/announce/0.13.elm

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

|]

