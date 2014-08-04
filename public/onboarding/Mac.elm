
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

  * [Using the Terminal](#using-the-terminal)
  * [Creating a Project](#creating-a-project)
  * [Next Steps](#next-steps)

## Using the Terminal

First, find and open an application called
[Terminal](http://en.wikipedia.org/wiki/Terminal_(OS_X)).

<img src="/onboarding/mac/terminal.png"
     style="width:520px; height:300px; display:block; margin:20px auto;">

 [cli]: http://en.wikipedia.org/wiki/Command_line_interface

Think of Terminal as a [text-based][cli] version of Finder. It lets you navigate
through folders and start programs, but it is all done with the keyboard.
It definitely has a learning curve, but it is really helpful when you are
programming. Here are a few helpful commands to get you started navigating
around your computer with Terminal:

  * `ls` &mdash; **l**i**s**t all the files and directories in the current directory.
    Directory is just another word for folder, so this is like asking Finder
    &ldquo;what is in this folder?&rdquo;

  * `cd` &mdash; **c**hange **d**irectories. Running `cd myProject` would move
    you into a subdirectory called `myProject`, assuming that directory exists.
    From there you could run `cd ..` which moves you out of a directory.

  * `mkdir` &mdash; **m**a**k**e a new **dir**ectory. Running `mkdir test` will
    create a new directory called `test`. It is the same as clicking "New Folder"
    and naming it `test`.

These are the basics of navigating around. Try them out to get comfortable with
them. Also, try pressing TAB which will often try to autocomplete whatever you
are typing at the moment. Finally, remember that everything is case sensitive!

## Creating a Project

In this section we will walk through creating an Elm project and developing
with `elm-server`. First we want to create a fresh directory to put all of our
Elm files in by running the following commands in your terminal one at a time:

```bash
cd ~/Desktop
mkdir elm
cd elm
```

We just navigated to your desktop, created a directory called `elm`, and moved
into that directory. Now we are going to create our first Elm file&mdash;called
`HelloWorld.elm`&mdash;by running these commands in your terminal:

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

Navigate to the [Learn](/Learn.elm) and [Examples](/Examples.elm) pages to
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
