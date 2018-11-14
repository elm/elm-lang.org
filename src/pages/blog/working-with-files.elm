import Html exposing (..)
import Html.Attributes exposing (..)

import Skeleton
import Center


main =
  Skeleton.blog
    "Working with Files"
    "(and accidentally simplifying the HTTP package)"
    Skeleton.evan
    (Skeleton.Date 2018 11 14)
    [ Center.markdown "600px" content
    ]


content = """

Elm has a nice new package for working with files! For example, if you want people to upload zip files, you would `elm install elm/file` and then write some code like this:

```elm
import File.Select as Select

type Msg
  = ZipRequested
  | ZipLoaded File

selectZip : Cmd Msg
selectZip =
  Select.file ["application/zip"] ZipLoaded
```

From there you can use functions from [`elm/file`][file], [`elm/bytes`][bytes], and [`elm/http`][http] to take the next steps. Maybe you want `File.size` to get the size in bytes? Maybe you want `File.toBytes` start working with the file content? Maybe you want `Http.fileBody` to send it along to some server?

I made a couple examples to make sure everything fits together nicely:

- Select files - [example](https://ellie-app.com/3SYtjPmN7Wja1)
- Drag-and-drop files - [example](https://ellie-app.com/3T5mNms7SwKa1)
- Drag-and-drop files with image previews - [example](https://ellie-app.com/3T5pn2qH7Jba1)
- Select files and then upload with progress - [code](https://github.com/elm/file/blob/master/examples/SelectFilesWithProgress.elm)
- Select files and then upload with progress and cancellation - [code](https://github.com/elm/file/blob/master/examples/SelectFilesWithProgressAndCancellation.elm)

You can play with all of these examples locally by following the instructions [here](https://github.com/elm/file/tree/master/examples).

As some of those examples demonstrate, the new `elm/file` and `elm/bytes` packages required changes in the existing `elm/http` package. How can we send files? Send bytes? Receive bytes? Etc. I was very surprised to find that these new requirements led to a _simpler_ HTTP package!

[bytes]: https://package.elm-lang.org/packages/elm/bytes/latest
[file]: https://package.elm-lang.org/packages/elm/file/latest
[http]: https://package.elm-lang.org/packages/elm/http/latest


## Simplifying `elm/http`

I have always found it difficult to introduce commands in [The Official Guide](https://guide.elm-lang.org/). Should I show random number generators first? How do I present JSON decoders? It never really fit together perfectly because HTTP is the obvious motivation for everyone to learn commands, but it felt a bit too complicated to start with. Point is, I really did not want these new (more niche) HTTP features to make this section even harder!

So I started working on adding the new features with the added requirement that the simple cases should stay simple. In the end, the simplifications end up looking like this:

```elm
-- BEFORE
getBook : Cmd Msg
getBook =
  Http.send GotText <|
    Http.getString "https://elm-lang.org/assets/public-opinion.txt"

-- AFTER
getBook : Cmd Msg
getBook =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotText
    }
```

No more making a `Request` that you must later `send`. Now you just create commands directly with `Http.get`. This style works especially well with `Http.post` and `Http.request` where there are more options to mess with. I already updated The Official Guide to account for these changes, so you can see more examples of these changes [here][1] and [here][2].

The new API seems pretty obvious in retrospect, but it was very difficult to find! I figure some readers may like to hear about that design process a bit.

[1]: https://guide.elm-lang.org/effects/http.html
[2]: https://guide.elm-lang.org/effects/json.html


## Design Process

My API design process always starts with deleting everything in the file except the types. The result often fits on screen without any scrolling. I find this extremely helpful in understanding how everything fits together, enabling quick experimentation. &ldquo;If I make a change here, it needs some tweaks over there. Ah, but then these errors need to be handled in a silly way...&rdquo; Once I stop seeing problems in the API, I show it to someone I trust in case they see problems. Maybe they know some use case I was not considering. I then try to implement it and see if _that_ reveals problems. Maybe there was some weird detail we missed. And once I implement it, I write the docs to be read linearly and see if _that_ reveals problems. When I feel like the docs are getting too complex, I go back to shuffling types around to see if it is an API problem. This is where I usually discover the best simplifications.

I find this process to be something of an emotional roller coaster, swinging between excitement and despair. It seems to produce the best results when I stay on until the end though. In this particular case, I ended up going through this loop many times because I could not get errors to work nicely for both commands and tasks, especially in the presence of different types of response content and progress tracking. The trick was to account for commands and tasks separately. Seeing the difference between “the same” and “similar” can be so hard!


## Thanks

Thank you to the folks who talked through various drafts of these packages. It was extremely helpful!

"""
