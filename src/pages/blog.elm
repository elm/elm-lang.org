import Html exposing (..)

import Center
import Skeleton


main =
  Skeleton.skeleton "Elm - Blog" Skeleton.Blog [ Center.markdown "600px" blog ]


blog = """


# Blog

### Articles

 * [Working with Files](/blog/working-with-files)
 * [Small Assets without the Headache](/blog/small-assets-without-the-headache)
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
 * [Concepts behind the Elm Logo](https://prezi.com/oqd48bv5ef0s/tangrams-logo/)
 * [Elm in VentureBeat](http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/)
 * [Elm &hearts; Prezi](/blog/announce/elm-and-prezi)
 * [Escape from Callback Hell](/blog/escape-from-callback-hell)
 * [Making Pong](/blog/making-pong)

<br>

### Release Notes

<table class="releases">
  <tbody>
    <tr>
      <td>[0.19](/blog/small-assets-without-the-headache)</td>
      <td>Smaller assets, faster compiler</td>
      <td>Aug 2018</td>
    </tr>
    <tr>
      <td>[0.18](/blog/the-perfect-bug-report)</td>
      <td>New debugger with session import/export</td>
      <td>Nov</td>
    </tr>
    <tr>
      <td>[0.17](/blog/farewell-to-frp)</td>
      <td>Add subscriptions, remove signals</td>
      <td>May 2016</td>
    </tr>
    <tr>
      <td>[0.16](/blog/compilers-as-assistants)</td>
      <td>Even better error messages!</td>
      <td>Nov</td>
      </tr>
    <tr>
      <td>[0.15.1](/blog/compiler-errors-for-humans)</td>
      <td>Dramatically improved error messages</td>
      <td>Jun</td>
    </tr>
    <tr>
      <td>[0.15](/blog/announce/0.15)</td>
      <td>Tasks, better HTTP library</td>
      <td>Apr</td>
    </tr>
    <tr>
      <td>[0.14.1](https://groups.google.com/forum/#!topic/elm-announce/6zRwjN68Ap0)</td>
      <td>HTML through main</td>
      <td>Jan 2015</td>
    </tr>
    <tr>
      <td>[0.14](/blog/announce/0.14)</td>
      <td>Package manager, parallel builds, JSON</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.13](/blog/announce/0.13)</td>
      <td>Debugging with elm-reactor</td>
      <td>Sep</td>
    </tr>
    <tr>
      <td>[0.12.3](/blog/announce/0.12.3)</td>
      <td>3D rendering with WebGL</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.12.1](/blog/announce/0.12.1)</td>
      <td>Fast Immutable Arrays</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.12](/blog/announce/0.12)</td>
      <td>Interactive UI Elements</td>
      <td>Mar</td>
    </tr>
    <tr>
      <td>[0.11](/blog/announce/0.11)</td>
      <td>Drastically improved FFI with ports</td>
      <td>Jan 2014</td>
    </tr>
    <tr>
      <td>[0.10.1](/blog/announce/0.10.1)</td>
      <td>Package manager integration</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.10](/blog/announce/0.10)</td>
      <td>Strings, Colors, Operators</td>
      <td>Oct</td>
    </tr>
    <tr>
      <td>[0.9](/blog/announce/0.9)</td>
      <td>Fast and reliable type inference</td>
      <td>Aug</td>
    </tr>
    <tr>
      <td>[0.8](/blog/announce/0.8)</td>
      <td>HTML/JS integration</td>
      <td>May</td>
    </tr>
    <tr>
      <td>[0.7.1](/blog/announce/0.7.1)</td>
      <td>Libraries for touch, either, and keyboard</td>
      <td>Feb</td>
    </tr>
    <tr>
      <td>[0.7](/blog/announce/0.7)</td>
      <td>Extensible records</td>
      <td>Jan 2013</td>
    </tr>
    <tr>
      <td>[0.6](/blog/announce/0.6)</td>
      <td>Whitespace sensitivity</td>
      <td>Dec</td>
    </tr>
    <tr>
      <td>[0.5](/blog/announce/0.5)</td>
      <td>Libraries for dictionaries, sets, and automata</td>
      <td>Oct</td>
    </tr>
    <tr>
      <td>[0.4](/blog/announce/0.4)</td>
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

<br>

### Conference Videos

* [Let’s be Mainstream!](http://www.elmbark.com/2016/03/16/mainstream-elm-user-focused-design) &mdash; 2015<br>“If functional programming is so great, why is it still niche? We have a product that can practically eliminate runtime errors, make refactoring much easier, lighten the testing burden, all while being quite delightful to use. What’s the hold up?”

* [What is Success?](https://youtu.be/uGlzRt-FYto) &mdash; 2018<br>Is it GitHub stars? Maximizing package downloads? Weekly blog posts? Adding ever more features? This talk gets into the implicit values that people bring to Elm, and tries to outline what is important in the Elm community.

* [Code is the Easy Part](https://youtu.be/DSjbTC-hvqQ) &mdash; 2016<br>People often think that contributing to an open-source project is strictly about adding code and adding features. This creates some really unhealthy incentives and interactions. This talk emphasizes the importance of personal relationships, helping each other, and community participation as the foundation of a fruitful collaboration.

* [The Life of a File](https://youtu.be/XpDsk374LDE) &mdash; 2017<br>Many folks get anxious about their project structure. “If I get it wrong, I am doomed!” This talk outlines the recommended approach for growing a large codebase. With the compiler making refactors easy, it is not as hard as you might think!

* [Accidentally Concurrent](https://youtu.be/DfLvDFxcAIA) &mdash; 2015<br>This talk examines references, objects, and reactivity in terms of concurrency. This reframing is a useful way of understanding the “accidental complexity” in your code base. ([abstract](http://www.codemesh.io/codemesh2015/evan-czaplicki))

* [Controlling Time and Space](https://youtu.be/Agu6jipKfYw) &mdash; 2015<br>Categorizes the many formulations of FRP, showing how they relate to Elm and what benefits you get from doing it the Elm way.

* [Functional Reactive Programming in Elm](http://www.infoq.com/presentations/elm-reactive-programming) &mdash; 2013<br>Teaches the basics of graphics and FRP in Elm and builds up to implementing a basic [Mario game](/examples/mario) *and* it is pretty fun to watch.

* [Elm: Making the Web Functional](http://www.infoq.com/presentations/Elm) &mdash; 2012<br>First conference talk ever. Covers the basics of graphics and FRP before Elm even had its record system!

[mlocjs]: http://www.ustream.tv/recorded/29330499

"""
