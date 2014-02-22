import Website.Skeleton (skeleton)
import Window

main = lift (skeleton content) Window.dimensions

content w = width w [markdown|
<style>
h1 { margin-bottom: 0; }
ul { margin-top: 0; }
h2,h3,h4 { margin-bottom: 0.5em; margin-top: 2em; }
h5 { margin-bottom: 0.5em; }
</style>

# Learning Roadmap

This page provides advice on how to learn Elm based on your background.
Multiple sections may apply to you, so keep reading even if you found a
good starting point!

And if you are coming to Elm from Haskell or JavaScript, see these
[tips](/learn/From-other-language-tips.elm).

### New to programming

 * watch the [beginner classes](/Learn.elm#beginner-classes) and do the exercises!

### New to functional programming

 * [syntax reference](/learn/Syntax.elm)
 * [feature tutorials](/Learn.elm#features)
 * [examples of functional concepts](/Examples.elm#compute)

### New to Functional Reactive Programming

 * [an introduction to FRP](/learn/What-is-FRP.elm)
 * [basic examples of FRP in action](/Examples.elm#react)
 * [video explaning FRP](http://www.infoq.com/presentations/elm-reactive-programming)
   in a fun and accessible way
 * [thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)
   explaining how FRP works in Elm, very accessible despite the format!

### Using Elm

 * see the [intermediate](/Examples.elm#intermediate) and [big examples](/Examples.elm#big-examples)
 * [skeletons for making websites and games](/Learn.elm#making-stuff)
 * [release notes](/Install.elm#release-notes) which may contain information that has not yet made it into docs
 * [blog posts](#articles-blog)
|]
