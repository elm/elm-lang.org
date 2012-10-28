
import Website.Skeleton

intro = [markdown|

<h1><div style="text-align:center">Escape from Callback Hell
<div style="font-size:0.5em;font-weight:normal">*Callbacks are the modern `goto`*</div></div>
</h1>


Just like `goto`, callbacks lead to non-linear code that
is hard to read, maintain, and reason about. And just like
with `goto`, this problem can be solved by introducing higher
level control flow mechanisms. So if you want to escape from
Callback Hell, you need to understand [Functional Reactive Programming][frp] (FRP).

This post is intended to highlight the problems with callbacks, show how FRP
solves these problems, and ultimately convince you that:

<div style="text-align:center">
[goto][goto] &nbsp; **:** &nbsp; [structured programming][struct] &nbsp; **: :** &nbsp; [callbacks][callback] &nbsp; **:** &nbsp; [reactive programming][frp]
</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"
  [callback]: http://en.wikipedia.org/wiki/Callback_(computer_programming) "Callbacks"
  [frp]: /learn/What-is-FRP.elm "FRP"
  [struct]: http://en.wikipedia.org/wiki/Structured_programming "Structured Programming"

|]

quote1 = spacer 170 180 `above` width 170 [markdown|

<div style="color:#666;font-size:0.6em;text-align:center">
&ldquo;The unbridled use of the go to statement has an immediate consequence that it
becomes terribly hard to find a meaningful set of coordinates in which to describe the process progress.&rdquo;
</div>
<br/>
<div style="color:#666;font-size:0.6em;text-align:right">&mdash; Edsger Dijkstra</div>

|]

quote2 = spacer 170 20 `above` width 170 [markdown|

<div style="color:#666;font-size:0.6em;text-align:center">
&ldquo;For a number of years I have been familiar with the observation that the quality
of programmers is a decreasing function of the density of go to statements in the programs they produce.&rdquo;
</div>
<br/>
<div style="color:#666;font-size:0.6em;text-align:right">&mdash; Edsger Dijkstra</div>

|]

midtro1 = [markdown|

### The Problem

If you have worked with [AJAX][ajax] or [node.js][nodejs] or any other callback heavy
framework, you have probably been to Callback Hell. Your whole application ends up
being passed around as callbacks, making the code extremely difficult to read and
maintain. Borrowing terminology from the days of `goto` when the flow of control
easily became a tangled mess, modern callback-heavy code is often pejoritively called
[spaghetti code][spaghetti].

  [ajax]: http://en.wikipedia.org/wiki/Ajax_(programming) "AJAX"
  [nodejs]: http://nodejs.org "node.js"
  [spaghetti]: http://en.wikipedia.org/wiki/Spaghetti_code "Spaghetti code"

Just like `goto`, these callbacks force you to jump around your codebase
in a way that is really hard to understand. You basically have
to read the whole program to understand what any individual function does.

And good luck if you want to add something to your code. A change in one function
may break functions that *appear* to be unrelated (neither function calls the other).
You'll usually find yourself carefully tracing through the entire sequence of
callbacks to find out what your change will really do.

If you are not convinced that callbacks and `goto` are comparably bad practices,
read Edsger Dijkstra's famous [&ldquo;Go To Statement Considered Harmful&rdquo;][goto]
and replace the mentions of `goto` with mentions of callbacks.

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

Okay, so there are a lot of things to dislike about callbacks, but they *do* serve a
vital role in modern programs. In an effort to reach a better solution we must ask:
why do we *really* need callbacks? What task do they perform? What is their fundamental
role in our programs?

We often want to say, &ldquo;When this value is ready, take this action.&rdquo;
This is a time-dependent relationships. One value depends on another
that changes over time. We want to say, &ldquo;While this is happening,
that can happen too.&rdquo; This is a time-dependent relationship too.
These computatinos can happen concurrently. 
These time relationships are not covered by traditional control structures,
so we use the modern `goto` to work around it.

|]

midtro2 = [markdown|

[Functional Reactive Programming][frp] (FRP) explicitly models &ldquo;changes over time&rdquo;,
providing a high-level framework for describing time-dependent relationships.
It formalizes these relationships, resulting in simple syntax and semantics.
FRP lets you make asynchronous calls without the callbacks. Without the non-linear
control flow. Without the headache. You can write code that is both responsive *and* readable.

  [frp]: /learn/What-is-FRP.elm "FRP"

To understand the existing problem and how it is solved with FRP, let's make
this more concrete.

### Case Study: Using the Flickr API

Flickr &ndash; a photo sharing service &ndash; exposes an [API][api] that allows
you to programmatically find and download photos. We want to find an image with
a user defined tag. So if I ask for &ldquo;Tokyo&rdquo; I should get back a
random image with a Tokyo tag.

This requires two explicit HTTP requests to Flickr, one to find some tagged photos
and another to get the size options for a given image.

We will perform this task three ways: synchronously, asynchronously with callbacks,
and asynchronously with FRP.

  [api]: http://www.flickr.com/services/api/ "Flickr API"

Each of these case studies will use the following functions:

  - `requestTag` which turns a tag &ndash; such as `"badger"` &ndash; into
    a valid Flickr API request. These requests will return a JSON object
    containing `"badger"` photos.

  - `requestOneFrom` takes a JSON object of photos and turns it into a request
    for just *one* photo. This request will return a JSON object of size options.
    Flickr is basically asking, &ldquo;Do you want low resolution? Original quality?
    800 by 600? ...&rdquo; This function also handles any errors that might have
    occured with the previous request.

  - `sizesToPhoto` turns a JSON object of sizes options into an actual image that
    we can use. Again, this function handles any errors that might have occured with
    the previous request.

  - `drawOnScreen` puts the image on screen for the user to see. You do not need to
    mess around with the DOM in Elm, so this function only gets used in the JS code.

Great! With the core logic under control, let's see how our code looks in three different
scenarios: synchronous, asynchronous with callbacks, and asynchronous with FRP.

#### 1. Readable but Unresponsive

First let's do this with synchronous HTTP requests. Here is our code:

        function getPhoto(tag) {
            var photoList = syncGet(requestTag(tag));
            var photoSizes = syncGet(requestOneFrom(photoList));
            return sizesToPhoto(photoSizes);
        }

        drawOnScreen(getPhoto('tokyo'));

It's pretty clear what is going on here. The `syncGet` function takes a request and blocks
until it recieves a response. The logic of the program is simple and linear. If you wanted
to add anything, it is pretty obvious where it would go. You never have moments where you
need a value but it does not exist yet.

This code is really nice to read, but it will make your whole application freeze for the duration
of the HTTP requests. It basically dodges the issue of time-dependence by just freezing if it is
waiting for a value. This blocks everything in the program. Mouse and keyboard input just piles
up, waiting to be processed, presenting the user with an unresponsive app. This is not an
acceptable user experience.


#### 2. Responsive but Unreadable

The current solution is to instead make asynchronous HTTP requests (AJAX requests) that use
callbacks that give finer-grained control over time-dependencies.

        function getPhoto(tag, handlerCallback) {
            asyncGet(requestTag(tag), function(photoList) {
                asyncGet(requestOneFrom(photoList), function(photoSizes) {
                    handlerCallback(sizesToPhoto(photoSizes));
                });
            });
        }
    
        getPhoto('tokyo', drawOnScreen);

The `asyncGet` function takes a request and a callback to run once a response is recieved.
We can now say, &ldquo;These computations must happen one after another, but other things
can happen in the meantime.&rdquo;

Yay, we solved the problem of the site freezing, but now our code is a mess of deeply
nested functions that is annoying to write and unpleasant to read and maintain.
This is a glimpse into Callback Hell.

We have effectively done a manual conversion to [continuation passing style (CPS)][cps],
a common compiler technique for functional languages.

CPS is great for compilers because it is easy to map onto assembly-level jumps (i.e. `goto`),
helping [make functional languages efficient][cps_compile]! CPS is terrible for humans for
the same reason. It is pretty much the same as using `goto` to structure your programs.

 [cps_compile]: http://matt.might.net/articles/cps-conversion/ "Compiling with Continuations"
 [cps]: http://matt.might.net/articles/by-example-continuation-passing-style/ "cps"

|]

midtro3 = [markdown|

#### 3. Responsive *and* Readable

[Functional Reactive Programming][frp] uses *signals*, values that change over time, to represent
all interactive time-varying content. For example, the position of the mouse is a
&ldquo;signal&rdquo; because it changes over time. FRP provides functions such as:

        lift :: (a -> b) -> Signal a -> Signal b

The `lift` function applies a given function to a time-varying value whenever that value changes.
This creates a new time varying value `Signal b` that is time-dependent on `Signal a`. This
lets us write one-liners like:

  [frp]: /learn/What-is-FRP.elm "FRP"

|]

types = spacer 170 80 `above` width 170 [markdown|

<span style="color:#666;font-size:0.6em">
**How to read types:**<br/>The `::` character can be read as &ldquo;has type&rdquo;.
So we say that `(42 :: Int)` which means that 42 has type integer. The arrow `->` represents a function.
So that could be something like `(toFloat :: Int -> Float)` or `(not :: Bool -> Bool)`. 
The lower case letters are like wild cards, usually called *type variables*.
Just like normal variables, they can be anything but every occurance of a variabled is the same.
For instance, we say that `(==) :: a -> a -> Bool`, meaning that equality will work with any two
arguments of the same type.
</span>

<span style="color:#666;font-size:0.6em">
**Why types are cool:**<br/>The types create usage interfaces on a per function basis.
And with type inference, this does not require *any* annotation.
That means you can *safely* assume that the input you are getting is exactly what you expect.
You do not need to check for `null` or ensure that the input has the structure you expect.
The types ensure that these things are not a problem.
No one can break your code by providing `null` or providing an object instead of an array or
some other silly mistake where you end up with the wrong *type* of argument.
</span>

|]

example1 =
  let code = text . monospace $ toText "lift asText Mouse.position  ==>  " in
  lift (\p -> let ex = code `beside` asText p in container (widthOf ex) (heightOf ex) middle ex) Mouse.position

outro = [markdown|

where `(asText :: a -> Element)` turns any value into a viewable graphical
element (called an `Element`). And an `Element` that changes over time is
an animation! That means that `lift asText Mouse.position` results in an
animation that is tied directly to the mouse position.

There are [tons of time-dependent relationships][signal] that can be expressed with FRP. The one
we care about is the [`send`][http] relationship.

 [signal]: /docs/Signal/Signal.elm "Signal Docs"
 [http]: /docs/Signal/HTTP.elm "HTTP Docs"

        send :: Signal (Request String) -> Signal (Response String)

It takes in a time-varying request string. It outputs a time-varying
responses. These signals depend on each other, but they do not need to have the
same immediate time-dependency that we saw with `lift` where the second signal changes
immediately. With `send`, the signal of responses is updated only when
the response is ready. Most importantly, `send` is non-blocking.

Let's see the code:

        getPhotos :: Signal String -> Signal Element
        getPhotos tags =
            let photoList  = send (lift requestTag     tags     ) in
            let photoSizes = send (lift requestOneFrom photoList) in
                lift sizesToPhoto photoSizes

We have clear, linear control flow, but the resulting program automatically
optimizes for asynchrony. It does not block while waiting for a response and
our program is still well defined. We now have the readability of synchronous
code with the efficiency of asynchronous callbacks. There is no trade-off
between readability and responsiveness!


### Conclusions


|]

content w ex1 =
  let p0 = width w intro in
  let p1 = flow right [ width w midtro1, spacer 30 100, quote1 ] in
  let p2 = flow right [ width w midtro2, spacer 30 100, quote2 ] in
  let p3 = width w midtro3 in
  let p4 = width w outro in
  flow down
    [ p0, p1, p2
    , flow right [ flow down [ p3, width w ex1, p4 ]
                 , spacer 30 100
                 , types ] ]

defaultContent = content 600

blog ex1 w' = 
    let w = w' - 200 in
    let c = if w' == 800 then defaultContent ex1 else content w ex1 in
      container w' (heightOf c) middle c

everything = lift blog example1

main = lift2 skeleton everything Window.width