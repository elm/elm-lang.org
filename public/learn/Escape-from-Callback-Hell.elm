import Website.Skeleton (skeleton)
import Website.ColorScheme (lightGrey,mediumGrey)

import JavaScript as JS
import JavaScript.Experimental as JS
import Window
import Graphics.Input as Input
import Http
import Json

intro = [markdown|

<style type="text/css">
p { text-align:justify; }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: #f8f8f8; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: #f8f8f8; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

<h1><div style="text-align:center">Escape from Callback Hell
<div style="font-size:0.5em;font-weight:normal">*Callbacks are the modern `goto`*</div></div>
</h1>

|]

quote1 = spacer 170 200 `above` width 170 [markdown|

<div style="color:#666;font-size:0.8em;text-align:left">
&ldquo;The unbridled use of the go to statement has an immediate consequence that it
becomes terribly hard to find a meaningful set of coordinates in which to describe the process progress.&rdquo;
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:right">[Edsger Dijkstra][goto]</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

|]

quote2 = spacer 170 40 `above` width 170 [markdown|

<div style="color:#666;font-size:0.8em;text-align:left">
&ldquo;For a number of years I have been familiar with the observation that the quality
of programmers is a decreasing function of the density of go to statements in the programs they produce.&rdquo;
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:right">[Edsger Dijkstra][goto]</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

|]

midtro1 = [markdown|

Callbacks are used to structure programs. They let us say, &ldquo;When this value is ready,
go to another function and run that.&rdquo; From there, maybe you go to *another* function
and run that too. Pretty soon you are jumping around the whole codebase.

If you have worked with [AJAX][ajax] or [node.js][nodejs] or any other callback heavy
framework, you have probably been to Callback Hell. Your whole application ends up
being passed around as callbacks, making the code extremely difficult to read and
maintain. The resulting tangled mess of code is often pejoratively called
[spaghetti code][spaghetti], a term borrowed from the days of `goto`.

  [ajax]: http://en.wikipedia.org/wiki/Ajax_(programming) "AJAX"
  [nodejs]: http://nodejs.org "node.js"
  [spaghetti]: http://en.wikipedia.org/wiki/Spaghetti_code "Spaghetti code"

Just like `goto`, these callbacks force you to jump around your codebase
in a way that is really hard to understand. You basically have
to read the whole program to understand what any individual function does.

And good luck if you want to add something to your code. A change in one function
may break functions that *appear* to be unrelated (the functions may never even
appear together in the entire codebase, connected only by deeply nested callbacks).
You'll usually find yourself carefully tracing through the entire sequence of
callbacks to find out what your change will really do.

If you are not convinced that callbacks and `goto` are equally harmful,
read Edsger Dijkstra's famous [&ldquo;Go To Statement Considered Harmful&rdquo;][goto]
and replace the mentions of `goto` with mentions of callbacks.

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"

Okay, so there are a lot of things to dislike about callbacks, but they *do* serve a
vital role in modern programs. In an effort to reach a better solution we must ask:
why do we *really* need callbacks? What task do they perform? What is their fundamental
role in our programs?

Well we often want to say, &ldquo;When this value is ready, take this action.&rdquo;
This is a time-dependent relationship. We depend on a value as it changes over time.
We also want to say, &ldquo;While this is happening, that can happen too.&rdquo;
This is a time-dependent relationship too. These computations can happen concurrently. 
These time relationships are not covered by traditional control structures,
so we use the modern `goto` to work around it.


## A Preview of the Solution

Just like `goto`, callbacks lead to non-linear
code that is hard to read, maintain, and understand. And just like
with `goto`, this problem can be solved by introducing higher
level control flow mechanisms. So if you want to escape from
Callback Hell, you need to understand [Functional Reactive Programming][frp] (FRP).
In short:

<div style="text-align:center">
[goto][goto] &nbsp; **:** &nbsp; [structured programming][struct] &nbsp; **: :** &nbsp; [callbacks][callback] &nbsp; **:** &nbsp; [reactive programming][frp]
</div>

  [goto]: http://www.u.arizona.edu/~rubinson/copyright_violations/Go_To_Considered_Harmful.html "goto"
  [callback]: http://en.wikipedia.org/wiki/Callback_(computer_programming) "Callbacks"
  [frp]: /learn/What-is-FRP.elm "FRP"
  [struct]: http://en.wikipedia.org/wiki/Structured_programming "Structured Programming"

|]

midtro2 = [markdown|

[Functional Reactive Programming][frp] (FRP) is a high-level framework for
describing time-dependent relationships. FRP formalizes these time-dependencies,
resulting in simple syntax and semantics.

FRP lets you make asynchronous calls without the callbacks. Without the non-linear
control flow. Without the headache. You can write code that is both readable *and* responsive.

  [frp]: /learn/What-is-FRP.elm "FRP"

To understand the existing problem and how it is solved with FRP, let's make
this more concrete. The following example will explain the current state
of affairs and fully explain how to create readable, responsive code that is
entirely free of callbacks.

|]

funcs = spacer 170 300 `above` width 170 [markdown|

<div style="color:#666;font-size:0.8em;text-align:left">
**Function Specifications**
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:left">
`requestTag` which turns a tag &ndash; such as `"badger"` &ndash; into
a valid Flickr API request. These requests will return a JSON object
containing a list of `"badger"` photos.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:left">
`requestOneFrom` takes a JSON object of photos and turns it into a request
for just *one* photo. This request will return a JSON object of size options.
Flickr is basically asking, &ldquo;Do you want low resolution? Original quality?
800 by 600? ...&rdquo; This function also handles any errors that might have
occured with the previous request.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:left">
`sizesToPhoto` turns a JSON object of sizes options into an actual image that
we can use. Again, this function handles any errors that might have occured with
the previous request.
</div>
<div style="height:0.5em"></div>
<div style="color:#666;font-size:0.8em;text-align:left">
`drawOnScreen` puts the image on screen for the user to see. You do not need to
mess around with the DOM in [Elm](/), so this function only gets used in the JS code.
</div>

|]


midtro3 = [markdown|

## Case Study: Using the Flickr API

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

```javascript
function getPhoto(tag) {
    var photoList  = syncGet(requestTag(tag));
    var photoSizes = syncGet(requestOneFrom(photoList));
    return sizesToPhoto(photoSizes);
}

drawOnScreen(getPhoto('tokyo'));
```

It's pretty clear what is going on here. The `syncGet` function takes a request and blocks
until it receives a response. The logic of the program is simple and linear. If you wanted
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

```javascript
function getPhoto(tag, handlerCallback) {
    asyncGet(requestTag(tag), function(photoList) {
        asyncGet(requestOneFrom(photoList), function(photoSizes) {
            handlerCallback(sizesToPhoto(photoSizes));
        });
    });
}
    
getPhoto('tokyo', drawOnScreen);
```

The `asyncGet` function takes a request and a callback to run once a response is received.
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

#### 3. Readable *and* Responsive

[Functional Reactive Programming][frp] uses *signals*, values that change over time, to represent
all interactive time-varying content. For example, the value of a text input field is a
&ldquo;signal&rdquo; because it changes over time. We can create such a signal with:

  [frp]: /learn/What-is-FRP.elm "FRP"

```haskell
(tagField, tags) = Input.field "Tag"
```

We created two values. The first is a signal of visual elements called `tagField`. This is a normal
text box. The second is a signal called `tags`. The value of `tags` changes automatically
as the user types and highlights in the input field. Here they are in action. Try typing
into the input box to see `tags` update automatically.

|]

asyncElm2 = [markdown|
We can then do all sorts of computations with `tags`. When `tags` changes
the signals that depend on it change automatically:
|]

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

```haskell
getPhotos tags =
    let photoList  = send (lift requestTag tags)
        photoSizes = send (lift requestOneFrom photoList)
    in  lift sizesToPhoto photoSizes
```

We have effectively set up a processing pipeline of how to handle user input:
we take in a tag, turn it into a request, send it, turn the response into a
new request, send it, and finally turn *that* response into an image!

We have clear, linear control flow, and the resulting program automatically
optimizes for asynchrony. It does not block while waiting for a response and
our program is still well defined. We now have the readability of synchronous
code with the efficiency of asynchronous callbacks. There is no trade-off
between readability and responsiveness!

And because it is so easy to work with graphics in Elm, we can create a
workable Flickr search interface with only four additional lines of code!
The [full source code and interface can be seen here][flickr], with a more
idiomatic implementation of `getPhotos` that takes only one line. Take a look!
All of the graphics code lives in the definition of `scene`, taking up a
grand total of two lines. All told, [the core logic][flickr] is seven lines
from start to finish.

 [flickr]: /edit/examples/Intermediate/Flickr.elm "Flickr API Example"

In fact, here is an abbreviated version that fits in this blog post:

```haskell
scene img = flow down [ container 300  60 middle inputField
                      , fittedImage 300 300 img ]

main = lift scene (getPhotos (dropRepeats tags))
```

In `scene` we stack two elements vertically so that they flow downward.
The first one is a 300 by 60 container with our `inputField` text field right in
the middle. The second is an image that is automatically cropped to
fit nicely in a 300 by 300 square. The `main` function is what actually
gets put on screen. The only new thing in this line is `dropRepeats`
which filters out any values that are repeated, cutting down the number
of HTTP requests we make. The result looks like this:


|]

(tagInput, flickrTags) = Input.field "Flickr Search"

getSources : Signal String -> Signal (Maybe String)
getSources tag = let photos = Http.send (getTag <~ tag)
                     sizes  = Http.send (getOneFrom <~ photos)
                 in  sizesToSource <~ sizes

scene : Element -> Maybe String -> Element
scene tagInput imgSrc =
    let img = case imgSrc of
                Just src -> fittedImage 300 300 src
                Nothing  -> color lightGrey (spacer 298 298)
    in  flow down [ container 300 60 middle tagInput,
                    color mediumGrey <| container 300 300 middle img ]


searchBox = scene <~ tagInput ~ getSources (dropRepeats flickrTags)


{---------------------  Helper Functions  ---------------------}

-- The standard parts of a Flickr API request.
flickrRequest args =
  "http://api.flickr.com/services/rest/?format=json" ++
  "&nojsoncallback=1&api_key=256663858aa10e52a838a58b7866d858" ++ args

-- Turn a tag into an HTTP GET request.
getTag : String -> Http.Request String
getTag tag =
    let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
    in  Http.get (if tag == "" then "" else flickrRequest args ++ tag)

toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing

-- Take a list of photos and choose one, resulting in a request.
getOneFrom photoList =
    case toJson photoList of
      Nothing -> Http.get ""
      Just json ->
          let photoRecord = JS.toRecord <| Json.toJSObject json
          in  case photoRecord.photos.photo of
                h::_ -> Http.get (flickrRequest "&method=flickr.photos.getSizes&photo_id=" ++ h.id)
                []   -> Http.get ""
                        
-- Take some size options and choose one, resulting in a URL.
sizesToSource sizeOptions =
    case toJson sizeOptions of
      Nothing   -> Nothing
      Just json ->
          let sizesRecord = JS.toRecord <| Json.toJSObject json
              sizes = sizesRecord.sizes.size
          in  case reverse sizes of
                _ :: _ :: _ :: _ :: _ :: size :: _ -> Just size.source
                size :: _ -> Just size.source
                _ -> Nothing

outro = [markdown|

## Conclusions

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

Look out for an upcoming post on **integrating Elm with JS code**. This
post will show how Elm can be used to handle your time dependent
logic without disrupting an existing codebase.
So you can use FRP to escape Callback Hell and still directly use JavaScript,
JQuery, Bootstrap, or any other existing web technology. Until then, you can
read up on the [JavaScript Event Interface][jsei] which makes this all possible.

  [jsei]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Event Interface"

If you interested in this approach, [download Elm][download] or
[experiment online](http://elm-lang.org/edit/examples/Basic.elm)!
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

(inputField, tags) = Input.field "Tag"

code = centered . monospace . toText
box w e = container w 40 middle e
pairing w left right = box (w `div` 2) left `beside` box (w `div` 2) right

requestTagSimple t = if t == "" then "" else "api.flickr.com/?tags=" ++ t
dropTil s = case s of { h::t -> if h == '[' then t else dropTil t ; _ -> s }
format r = map (\c -> if c == '"' then '\'' else c) (take 19 (dropTil r))
showResponse r =
  code <| case r of
            Http.Success r -> "Success \"... " ++ format r ++ " ...\""
            Http.Waiting -> "Waiting"
            Http.Failure n _ -> "Failure " ++ show n ++ " \"...\""

content inputField tags response search wid =
  let w = wid - 200
      sideBySide big small = flow right [ width w big, spacer 30 100, small ]
      (iw,ih) = sizeOf inputField
      input = color lightGrey <| container (iw+2) (ih+2) middle
                              <| color white inputField
      asyncElm = flow down . map (width w) <|
                 [ asyncElm1
                 , pairing w input (asText tags)
                 , asyncElm2
                 , pairing w (code "lift length tags") (asText <| length tags)
                 , pairing w (code "lift reverse tags") (asText <| reverse tags)
                 , pairing w (code "lift requestTag tags") (asText <| requestTagSimple tags)
                 , asyncElm3
                 , pairing w (code "send (lift requestTag tags)") (showResponse response)
                 , asyncElm4 ]
  in flow down
      [ width w intro
      , sideBySide midtro1 quote1
      , sideBySide midtro2 quote2
      , sideBySide midtro3 funcs
      , asyncElm
      , container w (heightOf search) middle search
      , width w outro
      ]

everything = content <~ inputField
                      ~ tags
                      ~ Http.send (getTag <~ tags)
                      ~ searchBox

main = lift2 skeleton everything Window.width

titles = constant (JS.fromString "Escape from Callback Hell")
foreign export jsevent "title"
  titles : Signal JS.JSString