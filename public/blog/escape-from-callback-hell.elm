
import Website.Skeleton

intro = [markdown|

<style type="text/css">p { text-align:justify; }</style>

<h1><div style="text-align:center">Escape from Callback Hell
<div style="font-size:0.5em;font-weight:normal">*Callbacks are the modern `goto`*</div></div>
</h1>


Just like `goto`, callbacks lead to non-linear code that
is hard to read, maintain, and understand. And just like
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

If this post proves convincing, the [Elm programming language](/) has full support for
functional reactive web programming, so you can start experimenting today.

|]

quote1 = spacer 170 200 `above` width 170 [markdown|

<div style="color:#666;font-size:0.6em;text-align:left">
&ldquo;The unbridled use of the go to statement has an immediate consequence that it
becomes terribly hard to find a meaningful set of coordinates in which to describe the process progress.&rdquo;
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:right">[Edsger Dijkstra][goto]</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

|]

quote2 = spacer 170 40 `above` width 170 [markdown|

<div style="color:#666;font-size:0.6em;text-align:left">
&ldquo;For a number of years I have been familiar with the observation that the quality
of programmers is a decreasing function of the density of go to statements in the programs they produce.&rdquo;
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:right">[Edsger Dijkstra][goto]</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

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
These computations can happen concurrently. 
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

|]


funcs = spacer 170 300 `above` width 170 [markdown|

<div style="color:#666;font-size:0.6em;text-align:left">
**Function Specifications**
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:left">
`requestTag` which turns a tag &ndash; such as `"badger"` &ndash; into
a valid Flickr API request. These requests will return a JSON object
containing a list of `"badger"` photos.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:left">
`requestOneFrom` takes a JSON object of photos and turns it into a request
for just *one* photo. This request will return a JSON object of size options.
Flickr is basically asking, &ldquo;Do you want low resolution? Original quality?
800 by 600? ...&rdquo; This function also handles any errors that might have
occured with the previous request.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:left">
`sizesToPhoto` turns a JSON object of sizes options into an actual image that
we can use. Again, this function handles any errors that might have occured with
the previous request.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em;text-align:left">
`drawOnScreen` puts the image on screen for the user to see. You do not need to
mess around with the DOM in [Elm](/), so this function only gets used in the JS code.
</div>

|]


midtro3 = [markdown|

### Case Study: Using the Flickr API

Flickr &ndash; a photo sharing service &ndash; exposes an [API][api] that allows
you to programmatically find and download photos. We want to find an image with
a user defined tag. So if I ask for &ldquo;Tokyo&rdquo; I should get back a
random image with a Tokyo tag.

  [api]: http://www.flickr.com/services/api/ "Flickr API"

This requires two explicit HTTP requests to Flickr, one to find some tagged photos
and another to get the size options for a given image.

We will perform this task three ways: synchronously, asynchronously with callbacks,
and asynchronously with FRP.

#### 1. Readable but Unresponsive

First let's do this with synchronous HTTP requests. We will rely on a couple of
functions specified to the right. Here is our code:

        function getPhoto(tag) {
            var photoList  = syncGet(requestTag(tag));
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

asyncElm1 = [markdown|

#### 3. Responsive *and* Readable

[Functional Reactive Programming][frp] uses *signals*, values that change over time, to represent
all interactive time-varying content. For example, the position of the mouse is a
&ldquo;signal&rdquo; because it changes over time.

  [frp]: /learn/What-is-FRP.elm "FRP"

        (inputField, tags) = Input.textField "Tag"

This creates two values. The first is a visual element called `inputField` that users can type into.
This is a normal text box. The second is a signal called `tags`. The value of `tags` changes automatically
as the user types into `inputField`. Here are the `inputField` and `tags` in action:

|]

asyncElm2 = [markdown|We can then do all sorts of computations with `tags`.|]

asyncElm3 = [markdown|

The `lift` function is used to transform a signal. It takes functions such as
`length`, `reverse`, or `requestTag` and applies them to a signal. This lets
us create new signals that automatically change whenever `tags` changes.

We are particularly interested in the third signal we created.
The code `(lift requestTag tags)` produces a Flickr request that
corresponds to the current user input (abbreviated for the sake of presentation).
Now we just need to send this request! For this we use the `send` function:

|]

asyncElm4 = [markdown|

Notice that when `tags` changes, the result of `send` does *not* change immediately.
The responses are sent and received asynchronously, so a response only arrives when
it is ready. In the meantime, your program is free to go about its normal business.

This is great because it allows us to have asynchrony without leaving the conceptual
framework of signals. The response is just another signal, exactly the same as `tags`.
We can turn its values into requests and send them too. In fact, that's exactly what we
are going to do. Here is the full Elm code for making many requests to the Flickr API:

        getPhotos tags =
            let photoList  = send (lift requestTag tags) in
            let photoSizes = send (lift requestOneFrom photoList) in
                lift sizesToPhoto photoSizes

We have effectively set up a workflow of how to handle user input:
turn a tag into a request, send it, turn the response into a new request,
send it, and finally turn *that* response into an image!

We have clear, linear control flow, and the resulting program automatically
optimizes for asynchrony. It does not block while waiting for a response and
our program is still well defined. We now have the readability of synchronous
code with the efficiency of asynchronous callbacks. There is no trade-off
between readability and responsiveness!

And because it is so easy to work with graphics in Elm, we can create a
workable Flickr search interface with only five additional lines of code!
The full source code and interface [can be seen here][flickr]. Take a look!
All of the graphics code lives in the definition of `scene`, taking up a
grand total of two lines.

 [flickr]: /edit/examples/Intermediate/Flickr.elm "Flickr API Example"

|]

{--
It takes in a time-varying request string. It outputs a time-varying
responses. These signals depend on each other, but they do not need to have the
instantaneous time-dependency that we saw with `lift` where the second signal changes
*immediately* after the first one. So with `send`, the signal of responses is updated
only when the response is ready. And most importantly, `send` is non-blocking.
Other code can execute while requests are in transit.

Let's see the code:

--}

types = spacer 170 80 `above` width 170 [markdown||]

{--
<div style="color:#666;font-size:0.6em">
**How to read types:**
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em">
The `::` can be read as &ldquo;has type&rdquo;.
So we say that `(42 :: Int)` which means that 42 has type integer. The arrow `(->)` represents a function.
So that could be something like `(not :: Bool -> Bool)` which means `not` takes a boolean and returns
a boolean. The lower case letters are like wild cards, usually called *type variables*.
They mean &ldquo;any type can go here&rdquo;. For instance, we say that `(==) :: a -> a -> Bool`,
meaning that equality takes two arguments of the same type.
</div>

<br/>
<br/>

<div style="color:#666;font-size:0.6em">
**Why types are useful:**
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.6em">
The types create interfaces on a per function basis.
That means you can *safely* assume that the input you are getting is exactly what you expect.
No one can break your code by providing `null`, providing an object instead of an array, or
some other silly mistake where you end up with an unexpected *type* of argument.
And with [type inference][infer], this does not require *any* type annotations.
</div>

 [infer]: http://en.wikipedia.org/wiki/Type_inference "Type Inference"

--}

outro = [markdown|

### Conclusions

Callbacks are the modern `goto`. Callbacks result in non-linear code that
is hard to read, maintain, and understand. Functional Reactive Programming
introduces high-level control structures that let you easily express
time-dependencies. The resulting code is both readable, responsive, and
significantly more concise.

Signals can express [many different time-relationships][signal],
such as dependence on the [present][clock] or [past][stamp],
time-sensitive [sampling][sample] and [filtering][filter], and [asynchrony][http].
This makes it much easier to deal with complicated time-dependent interactions,
a task that is extremely common when designing user interfaces.

  [signal]: /docs/Signal/Signal.elm "Signal Docs"
  [stamp]: /edit/examples/Intermediate/Stamps.elm "Stamps"
  [clock]: /edit/examples/Intermediate/Clock.elm "Clock"
  [sample]: /edit/examples/Reactive/SampleOn.elm "sampleOn"
  [filter]: /edit/examples/Reactive/KeepIf.elm "keepIf"
  [http]: /edit/examples/JavaScript/ZipCodes.elm "Zip Codes"

If you interested in this approach, [download Elm][download] and experiment!
Elm is only at version 0.5, but it already has lots of great [libraries](/Documentation.elm). If you feel like
there are some libraries or features missing, [you can help add them](/Contribute.elm).

  [download]: https://github.com/evancz/Elm/blob/master/README.md#elm "Dowload"

If you have questions or want to learn more, there are lots of helpful
resources. This [thesis][thesis] has much more information on how asynchrony
works in Elm. It also describes the history of FRP. It is quite accessible even
if you do not have any experience reading academic papers. You can also email
[the list][list] or ask questions on the [#elm channel at freenode][irc].

  [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "elm-discuss"
  [irc]: http://webchat.freenode.net/?channels=elm "#elm channel"

|]

(inputField, tags) = Input.textField "Tag"

code = text . monospace . toText
box w e = container w 30 middle e
pairing w left right = box (w `div` 2) left `beside` box (w `div` 2) right

requestTagSimple t = if t == "" then "" else "api.flickr.com/?tags=" ++ t
dropTil s = case s of { h:t -> if h == '[' then t else dropTil t ; _ -> s }
format r = map (\c -> if c == '"' then '\'' else c) (take 19 (dropTil r))
showResponse r = code (case r of { Success r -> "Success \"... " ++ format r ++ " ...\""
                                 ; Waiting -> "Waiting"
                                 ; Failure n _ -> "Failure " ++ show n ++ " \"...\"" })

content w tags response =
  let sideBySide big small = flow right [ width w big, spacer 30 100, small ] in
  let codePair l r = pairing w (code l) (asText r) in
  let asyncElm = flow down . map (width w) $
                 [ asyncElm1
                 , pairing w inputField (asText tags)
                 , asyncElm2
                 , codePair "lift length tags" (length tags)
                 , codePair "lift reverse tags" (reverse tags)
                 , codePair "lift requestTag tags" (requestTagSimple tags)
                 , asyncElm3
                 , pairing w (code "send (lift requestTag tags)") (showResponse response)
                 , asyncElm4 ] in
  flow down
    [ width w intro
    , sideBySide midtro1 quote1
    , sideBySide midtro2 quote2
    , sideBySide midtro3 funcs
    , sideBySide asyncElm types
    , width w outro ]

defaultContent = content 600

blog tags response w' = 
    let w = w' - 200 in
    let c = if w' == 800 then defaultContent tags response else content w tags response in
      container w' (heightOf c) middle c

requestTag tag =
  if tag == "" then "" else
  concat [ "http://api.flickr.com/services/rest/?format=json"
         , "&nojsoncallback=1&api_key=66c61b93c4723c7c3a3c519728eac252"
         , "&method=flickr.photos.search&sort=random&per_page=10&tags=", tag ]
  
everything = lift2 blog tags (HTTP.sendGet (lift requestTag tags))

main = lift2 skeleton everything Window.width

titles = constant (JavaScript.castStringToJSString "Escape from Callback Hell")
foreign export jsevent "elm_title"
  titles :: Signal JSString