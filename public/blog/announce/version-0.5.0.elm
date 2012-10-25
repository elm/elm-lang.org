
import JavaScript
import Window as Win
import Automaton
import Input
import Random
import List

title = constant (castStringToJSString "The Libraries You Need: Elm 0.5")
foreign export jsevent "elm_title"
  title :: Signal JSString

(butnMore,pressMore) = button "  +  "
(butnLess,pressLess) = button "  -  "

data Command = Incr | Decr | Idnt

formsAutomaton =
  let fstep (cmd,pos,color,mouse) fs =
          let fs' = case cmd of
                    { Incr -> fs ++ [draggable $ filled color (ngon 5 20 pos)]
                    ; Decr -> if fs == [] then [] else tail fs
                    ; Idnt -> fs }
          in  unzip $ map (\f -> step f mouse) fs'
  in  init' [draggable $ filled cyan (ngon 5 20 (200,200)) ] fstep

allInput tly wid =
  let { commands = let step less more = if less then Decr else
                                        if more then Incr else Idnt
                   in  lift2 step pressLess pressMore
      ; rand n = randomize 0 n commands
      ; pos = lift2 (,) (rand 400) (rand 400)
      ; color = lift3 rgb (rand 255) (rand 255) (rand 255)
      ; tlx = lift (\w -> (w - 400) `div` 2) wid
      ; relativePos = lift2 (\tlx (x,y) -> (x - tlx, y - tly)) tlx Mouse.position
      ; mouse = lift2 (,) Mouse.isDown relativePos }
  in  lift4 (,,,) commands pos color mouse

controls = 
  container 400 50 middle $
  flow right [ butnLess, plainText "  Number of Pentagons  ",  butnMore ]

display fs = collage 400 400 (outlined black (rect 400 400 (200,200)) : fs)

widget y w = lift (\forms -> display forms `above` controls)
           $ run formsAutomaton (allInput y w)

blog = [markdown|

# The Libraries You Need: Elm 0.5

This release focuses on growing [Elm](/)'s standard libraries to make sure you always have the tools you need.
For a full listing of Elm's current libraries, see [this page][docs].

  [docs]: /Documentation.elm "docs"

### Dictionaries and Sets

Elm now has [dictionaries][Dict] and [sets][Set]!

  [Dict]: /docs/Data/Dict.elm "Dictionary library"
  [Set]: /docs/Data/Set.elm "Set library"

The Dict and Set libraries could used from JavaScript. I can make this easier if people are interested. Let me know!

### Automatons

This version also introduces the [Automaton][auto] library. This library will
make it easier to create dynamic components that can be switched in and out of a program.

Most notably, it includes a `draggable` function. As seen in the following example,
it is pretty easy to dynamically create and destroy draggable forms.

  [auto]: /docs/Automaton.elm "Automaton Library"

|]



outro = [markdown|

That example uses draggable pentagons, but images, `Elements`, or any other shapes will work just as well.
Check out the [source][self] of this page if you want to mess around with it!

&ldquo;But what is an automaton?&rdquo; you might be asking. An automaton is like a little robot that
takes inputs and produces outputs. Without input, an automaton just sits quitely, waiting for something to do.
Automatons can have a &ldquo;memory&rdquo; so their output may depend on their past experiences. All automatons
are interchangeable, so they are easy to switch in and out of programs.

This library is based on the very clever ideas introduced by [Arrowized FRP][afrp].
I have made an effort to make it easier to understand and use for people unfamiliar with
&ldquo;Arrows&rdquo; and other concepts that are largely orthoganal to doing-things-in-real-life.
I am hoping that the term [&ldquo;automaton&rdquo;][wiki] is somewhat familiar (or at least
a better anology than &ldquo;arrow&rdquo;).

I plan on writing some blog posts on automatons, so hopefully that will make it clearer why they
are totally rad.

  [self]: /edit/blog/announce/version-0.5.0.elm "source"
  [wiki]: http://en.wikipedia.org/wiki/Automata_theory#Informal_description "automaton wiki"
  [afrp]: http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf "Arrowized FRP"

### Escape from &ldquo;Callback Hell&rdquo; (new HTTP library)

You can now make pretty much any kind of request you want ([docs][http]). You can specify verb, url, payload, and custom headers.

This library makes it possible to escape &ldquo;callback hell&rdquo;. The [`send`][send] function takes in a signal of
HTTP requests and produces a signal of responses. The responses only update when they are ready. You can use
it just like any other signal, but it updates asynchronously, so you can write nice code that is both readable
and efficient. No callbacks! No nested-callbacks! ...

I will be writing more about this library fairly soon
because I think it is an important and novel part of Elm.
JS developers struggle with &ldquo;callback hell&rdquo; on a daily basis, and
now they do not have to!

  [send]: /docs/Signal/HTTP.elm "send"
  [http]: /docs/Signal/HTTP.elm "HTTP docs"

### New Functions and Syntax

- Abbreviated notation for tuple functions:
    * `(,)  === (\\x y -> (x,y))`
    * `(,,) === (\\x y z -> (x,y,z))`
    * etc.
- New functions for converting strings to numbers. Great for text input boxes:
    * `readInt :: String -> Maybe Int`
    * `readFloat :: String -> Maybe Float`
- [`(complement :: Color -> Color)`][color] which computes complementary colors! Surprisingly difficult to do!

  [color]: /docs/Graphics/Color.elm "Color library"

### Fewer Library Prefixes

The library prefixes have pretty much all been removed. `Data.List` is now `List`, `Signal.Mouse` is now `Mouse`, etc.
The prefixes end up being more confusing than helpful. `Data.List` makes it sound like some special version for no reason.

This is a breaking change, but one that I think makes Elm a nicer language.
The fix is just a matter of taking some words out of your `import` statements,
but please email [the list][list] if you want assistance with this (e.g. a script or some advice).

### Bug fixes, Optimizations, and Error Messages

- Generated JS is more readable.
- Pattern matching is smaller and faster in generated JS.
- Fix a bug with pattern parsing. `Four _ _ B _` was parsed as `Four _ _ (B _)`.
- `String` is now treated as an alias of `[Char]`.
- Better type error reporting for ambiguous uses of variables and for variables in aliased modules.

## Get involved!

If you want to help create libraries, please email [the list][list] or just get started on
your own. Elm needs your support! I'd love to have a nice parsing library so web developers
will stop trying to parse XML with regular expressions. And there are tons of cool
graphical components that would be super nice to have, like general purpose navigation bars
and sidebars.

(As for naming the parser, perhaps `Parsely`&trade; to start a plant theme in Elm, or
perhaps `Parser` because that is a clear descriptive name. It's not actually trade marked
but I'd be really sad if someone stole such a nice name!)

I also recently started an `#elm` IRC channel at freenode, so feel free to come hang out
and chat. It will probably be a bit quite because it has not been announced until now, but
I'll join whenever I can!

  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "Elm-Discuss"

|]

page =
  let { top = flow down
               [ container 600 40 bottomRight (text . Text.link "/" $ toText "Home")
               , width 600 blog ]
      ; mid = widget (heightOf top) Win.width
      ; bot = flow down [ width 600 outro
                        , container 600 60 middle . text . Text.color (rgb 216 221 225) $
                          toText "&copy; 2011-2012 Evan Czaplicki" ]
      }
  in  lift (\m -> flow down [ top, container 600 (heightOf m) middle m, bot ]) mid

main = lift2 (\page w -> container w (heightOf page) midTop page) page Win.width

