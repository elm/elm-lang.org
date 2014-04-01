
import Website.Blog (skeleton)
import Window

port title : String
port title = "Successful Install!"

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
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

<br/>You have installed the Elm Platform, so you now have a bunch of helpful
[command line tools](http://en.wikipedia.org/wiki/Command-line_interface) to
help you develop Elm programs. This tutorial will teach you how to use them!

  * [Using the Terminal](#using-the-terminal)
  * [Creating a Project](#creating-a-project)
  * [Next Steps](#next-steps)

## Using the Terminal

First, find and open an application called
[Terminal](http://en.wikipedia.org/wiki/Terminal_(OS_X)).

<img src="/onboarding/mac/terminal.png"
     style="width:520px; height:300px; display:block; margin:20px auto;">

This is a [command line interface](http://en.wikipedia.org/wiki/Command_line_interface)
for running programs and navigating around your computer. It definitely has a
learning curve, but it is really helpful when you are programming. Here are a
few helpful commands to get you started navigating around your computer with
the terminal:

  * `pwd` &mdash; show your **p**resent **w**orking **d**irectory, where you
    are right now.
  * `ls` &mdash; see all of the files and directories in the current directory.
  * `cd` &mdash; **c**hange **d**irectories. Running `cd myProject` would move
    you into a subdirectory called `myProject`, assuming that directory exists.
    From there you could run `cd ..` which moves you out of a directory.

These are the basics of navigating around. Try them out to get comfortable with
them. Also, try pressing TAB which will often try to autocomplete whatever you
are typing at the moment. Finally, remember that everything is case sensitive!

## Creating a Project

In this section we will walk through creating an Elm project and developing
with `elm-server`. In your terminal, run the following commands one at a time:

```bash
cd ~/Desktop
mkdir elm
cd elm
```

We just navigated to your desktop, created a directory called `elm`, and moved
into that directory. We are going to write some code here!

```bash
touch HelloWorld.elm
open -t HelloWorld.elm
```

The `touch` command created a new file called `HelloWorld.elm`. The next command
opens the new file in TextEdit, an extremely simple text editor. Now it is time
to write our first Elm program! In the editor window that you just opened, copy
in the following Elm code and save the file:

```haskell
main = plainText "Hello World!"
```

Now return to the terminal and run this command:

```bash
elm-server
```

This will start a server in the current directory that will let you look at
files from the browser. Open your browser and navigate to
[http://localhost:8000](http://localhost:8000). You should see a listing of all
the files in `~/Desktop/elm/`. Click on
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

TextEdit is not the best way to edit code. See [this
page](/Install.elm#syntax-highlighting) for a few reasonable options that will
do syntax highlighting for Elm. There are a ton of options out there though,
so definitely ask around before picking something.

#### A Reasonable Workflow

When you are working on a project, first start `elm-server` in the directory so
you can always take a look at it in your browser. After `elm-server` is running,
you can set up your editor and browser side-by-side, like in
[the online editor](/try):

<img src="/onboarding/mac/side-by-side.png"
     style="width:576px; height:324px; display:block; margin:20px auto;">

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
