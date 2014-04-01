
import Website.Blog

port title : String
port title = "Successful Install"

main = lift (skeleton everything) Window.width

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w pageTitle
  , section intro
  ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Success!</div>
<div style="font-size: 1.5em;">You just installed Elm, now what?</div>
</div>
|]

intro = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>

<br/>Programming is becoming more interactive.
JavaScript proved that the development loop can be as short as refreshing
your browser. More recently, [Go](http://golang.org/) made fast compilation
possible at Google scale.
Articles like [Learnable Programming](http://worrydream.com/LearnableProgramming/) are
exploring the possibilities of interactive programming while projects like
[LightTable](http://www.lighttable.com/) develop the tooling for it.
Our tools are finally allowing the kind of interactivity that makes
programming more fun and more productive.

[Elm](/) is taking the next step by supporting
[*hot-swapping*](http://en.wikipedia.org/wiki/Hot_swapping#Software), the
key component in truly *interactive programming*. Before diving into
details, we need to define these two terms to help formalize where we are
going and the challenges we face:

* **[Interactive Programming](http://en.wikipedia.org/wiki/Interactive_programming)
  &ndash; coding with immediate feedback**<br/>
  Tighten the development loop by integrating compilation, error messages, documentation,
  testing, and more into the *process* of coding.
  This encompasses the general goals of Learnable Programming and LightTable.

* **[Hot-swapping](http://en.wikipedia.org/wiki/Hot_swapping) &ndash;
  modifying running code**<br/>This is a specific technique used by compilers
  and runtime systems to swap new functions and values into a program while it
  is running. This technique is required for fully Interactive Programming.

Interactive programming is the goal and hot-swapping is the primary technical
challenge. I make this distinction because *hot-swapping is not always possible*
and it cannot be done *reliably* in most of today&rsquo;s programming languages.

We will be exploring the limits of hot-swapping, and how language design is
the key to making it easy and reliable. Before digging into details, let&rsquo;s
see how hot-swapping works in Elm:

|]
