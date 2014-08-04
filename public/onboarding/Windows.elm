
import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Successful Install!"

main = skeleton "" everything <~ Window.dimensions

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

<br>

You have installed the Elm Platform, so you now have a bunch of helpful
command line tools to help you develop Elm programs. This tutorial will teach
you how to use them!

  * [Creating a Project](#creating-a-project)
  * [Next Steps](#next-steps)

## Creating a Project

In this section we will walk through creating an Elm project and developing
with `elm-server`. Open Notepad and copy in the following Elm code:

```haskell
main = plainText "Hello World!"
```

When you save the file, create a new folder on your desktop called `elm` and
save it as `HelloWorld.elm`. Now click on the `elm` folder on your desktop,
to see its contents. In the file explorer hold `Shift` and right click to see
the *extended* list of options:

<img src="/onboarding/windows/command.png"
     style="width:420px; height:320px; display:block; margin:20px auto;">

Choose "Open command window here" to open the command prompt in this particular
folder. In the command prompt, run the command:

```bash
elm-server
```

This will start a server in the current directory that will let you look at
files from the browser. Open your browser and navigate to
[http://localhost:8000](http://localhost:8000). You should see a listing of all
the files in the `elm/` folder. Click on
[HelloWorld.elm](http://localhost:8000/HelloWorld.elm) to see your first
program!

#### Edit Code

In your text editor, edit `HelloWorld.elm` to say something else. In your
browser, refresh the [HelloWorld.elm](http://localhost:8000/HelloWorld.elm)
page to see your changes.

#### Create New Files

In the `~/Desktop/elm/` directory, create a new file called `MouseTracker.elm`
with the following content:

```haskell
import Mouse

main = lift asText Mouse.position
```

Navigate to the new file in your browser to see it in action!

#### Continue Learning

Navigate to the [Learn](/Learn.elm) and [Examples](./Examples.elm) pages to
read tutorials and see tons of example programs. Try creating files in your
project and running them on your computer.

#### Get a better editor

Notepad is not the best way to edit code. See [this
page](/Install.elm#syntax-highlighting) for a few reasonable options that will
do syntax highlighting for Elm. There are a ton of options out there though,
so definitely ask around before picking something.

#### A Reasonable Workflow

When you are working on a project, first start `elm-server` in the directory so
you can always take a look at it in your browser. After `elm-server` is running,
you can set up your editor and browser side-by-side, like in
[the online editor](/try):

<img src="/onboarding/windows/side-by-side.png"
     style="width:512px; height:320px; display:block; margin:20px auto;">

Now you can edit and save your file, then switch over and refresh your browser
to see the changes.

## Next Steps

The Elm Platform comes with quite a few helpful tools in addition to
`elm-server`. This section will give a brief overview of them:

  * [`elm`](https://github.com/elm-lang/Elm) &mdash;
    this command line tool actually compiles Elm programs to HTML
    and JavaScript. It is the most general way to compile Elm code, so if your
    project becomes too advanced for `elm-server` you may want to start using
    the compiler directly.

  * [`elm-repl`](https://github.com/elm-lang/elm-repl) &mdash;
    REPL stands for [read-eval-print-loop][repl] which lets you play with small
    Elm expressions. The REPL can import code from your projects, so if you want
    to play around with a function burried deep inside a module, you can load it
    into the REPL and test it out. `elm-repl` eventually needs to evaluate
    JavaScript code, so for now you need to install [node.js](http://nodejs.org/)
    to use it.

  * `elm-get` &mdash; this tool lets you grab libraries from the [Elm Public
    Library](http://library.elm-lang.org/). The Public Library is a catalog of
    helpful projects written by the Elm community to solve common problems.

With each of these tools you can use the `--help` flag to get more information.
Each tool also has a README on [GitHub](http://github.com/elm-lang) that has
some helpful information.

  [repl]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

|]
