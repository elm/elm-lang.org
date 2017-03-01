import Html exposing (..)

import Center
import Skeleton


main =
  Skeleton.skeleton "blog" [ Center.markdown "600px" blog ]


blog = """


# Blog

### Articles

 * [Google Summer of Code 2017](/blog/google-summer-of-code-2017)
 * [The Perfect Bug Report](/blog/the-perfect-bug-report)
 * [Blazing Fast HTML, Round Two](/blog/blazing-fast-html-round-two)
 * [How to Use Elm at Work](/blog/how-to-use-elm-at-work)
 * [A Farewell to FRP](/blog/farewell-to-frp)
 * [New Adventures for Elm](/blog/new-adventures-for-elm)
 * [Compilers as Assistants](/blog/compilers-as-assistants)
 * [Compiler Errors for Humans](/blog/compiler-errors-for-humans)
 * [Time Travel made Easy](/blog/time-travel-made-easy)
 * [Blazing Fast HTML](/blog/blazing-fast-html)
 * [Elm&rsquo;s Time Traveling Debugger](http://debug.elm-lang.org)
 * [Elm package manager](/blog/announce/package-manager)
 * [Elm REPL](/blog/announce/repl)
 * [Hot-swapping in Elm](/blog/interactive-programming)
 * [Concepts behind the Elm Logo](https://prezi.com/npjjrmt_badc/tangrams-logo/)
 * [Elm in VentureBeat](http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/)
 * [Elm &hearts; Prezi](/blog/announce/elm-and-prezi)
 * [Escape from Callback Hell](/blog/escape-from-callback-hell)
 * [Making Pong](/blog/making-pong)

<br>

### Release Notes

<table class="releases">
  <tbody>
    <tr>
      <td>[0.18][18]</td>
      <td>New debugger with session import/export</td>
      <td>Nov</td>
    </tr>
    <tr>
      <td>[0.17][17]</td>
      <td>Add subscriptions, remove signals</td>
      <td>May 2016</td>
    </tr>
    <tr>
      <td>[0.16][16]</td>
      <td>Even better error messages!</td>
      <td>Nov</td>
      </tr>
    <tr>
      <td>[0.15.1][151]</td>
      <td>Dramatically improved error messages</td>
      <td>Jun</td>
    </tr>
    <tr>
      <td>[0.15][15]</td>
      <td>Tasks, better HTTP library</td>
      <td>Apr</td>
    </tr>
    <tr>
      <td>[0.14.1][141]</td>
      <td>HTML through main</td>
      <td>Jan 2015</td>
    </tr>
    <tr>
      <td>[0.14][14]</td>
      <td>Package manager, parallel builds, JSON</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.13][13]</td>
      <td>Debugging with elm-reactor</td>
      <td>Sep</td>
    </tr>
    <tr>
      <td>[0.12.3][123]</td>
      <td>3D rendering with WebGL</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.12.1][121]</td>
      <td>Fast Immutable Arrays</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.12][12]</td>
      <td>Interactive UI Elements</td>
      <td>Mar</td>
    </tr>
    <tr>
      <td>[0.11][11]</td>
      <td>Drastically improved FFI with ports</td>
      <td>Jan 2014</td>
    </tr>
    <tr>
      <td>[0.10.1][101]</td>
      <td>Package manager integration</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.10][10]</td>
      <td>Strings, Colors, Operators</td>
      <td>Oct</td>
    </tr>
    <tr>
      <td>[0.9][9]</td>
      <td>Fast and reliable type inference</td>
      <td>Aug</td>
    </tr>
    <tr>
      <td>[0.8][8]</td>
      <td>HTML/JS integration</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.7.1][71]</td>
      <td>Libraries for touch, either, and keyboard</td>
      <td>Feb</td>
    </tr>
    <tr>
      <td>[0.7][7]</td>
      <td>Extensible records</td>
      <td>Jan 2013</td>
    </tr>
    <tr>
      <td>[0.6][6]</td>
      <td>Whitespace sensitivity</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.5][5]</td>
      <td>Libraries for dictionaries, sets, and automata</td>
      <td>Oct</td>
    </tr>
    <tr>
      <td>[0.4][4]</td>
      <td>Markdown</td>
      <td>Sep</td>
    </tr>
    <tr>
      <td>0.3.6</td>
      <td>JSON support</td>
      <td>Aug</td>
    </tr>
    <tr>
      <td>0.3.5</td>
      <td>JavaScript FFI</td>
      <td>Jun</td>
    </tr>
    <tr>
      <td>0.3</td>
      <td>Modules</td>
      <td>Jun</td>
    </tr>
    <tr>
      <td>0.1</td>
      <td>Initial Release</td>
      <td>Apr 2012</td>
    </tr>
  </tbody>
</table>

  [3]:  http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/
  [4]:  /blog/announce/0.4
  [5]:  /blog/announce/0.5
  [6]:  /blog/announce/0.6
  [7]:  /blog/announce/0.7
  [71]: /blog/announce/0.7.1
  [8]:  /blog/announce/0.8
  [9]:  /blog/announce/0.9
  [10]: /blog/announce/0.10
  [101]: /blog/announce/0.10.1
  [11]: /blog/announce/0.11
  [12]: /blog/announce/0.12
  [121]: /blog/announce/0.12.1
  [123]: /blog/announce/0.12.3
  [13]: /blog/announce/0.13
  [14]: /blog/announce/0.14
  [141]: https://groups.google.com/forum/#!topic/elm-announce/6zRwjN68Ap0
  [15]: /blog/announce/0.15
  [151]: /blog/compiler-errors-for-humans
  [16]: /blog/compilers-as-assistants
  [17]: /blog/farewell-to-frp
  [18]: /blog/the-perfect-bug-report

<br>

### Conference Videos

* [Let’s be Mainstream!][curry-on] &mdash;
  “If functional programming is so great, why is it still niche? We have a
  product that can practically eliminate runtime errors, make refactoring much
  easier, lighten the testing burden, all while being quite delightful to use.
  What’s the hold up?”

* [Accidentally Concurrent][cm2015] &mdash; This talk examines references,
  objects, and reactivity in terms of concurrency. This reframing is a
  useful way of understanding the “accidental complexity” in your code
  base. [Full Abstract][cm2015-abstract]

* [Controlling Time and Space][sl2014] &mdash; Categorizes the many
  formulations of FRP, showing how they relate to Elm and what benefits you
  get from doing it the Elm way.

* [Functional Reactive Programming in Elm][sl2013] &mdash; Teaches the basics
  of graphics and FRP in Elm and builds up to implementing a basic [Mario
  game](/examples/mario) *and* it is pretty fun to watch.

* [Elm: Making the Web Functional][sl2012] &mdash; First conference talk ever.
  Covers the basics of graphics and FRP before Elm even had its record system!

[curry-on]: http://www.elmbark.com/2016/03/16/mainstream-elm-user-focused-design
[cm2015]: https://youtu.be/DfLvDFxcAIA
[cm2015-abstract]: http://www.codemesh.io/codemesh2015/evan-czaplicki
[sl2014]: https://youtu.be/Agu6jipKfYw
[sl2013]: http://www.infoq.com/presentations/elm-reactive-programming
[mlocjs]: http://www.ustream.tv/recorded/29330499
[sl2012]: http://www.infoq.com/presentations/Elm

"""