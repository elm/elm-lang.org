# Template for creating Elm websites

This project contains all of the source files used to create
[elm-lang.org](http://elm-lang.org/). It provides a server and general
structure for creating *your own* Elm website. The server lets you mix
Elm, images, videos, HTML/CSS/JS, and whatever else you need.

You can also use this to run [elm-lang.org/try](http://elm-lang.org/try) locally.

### Set up

First make sure that you have the Elm compiler installed
([directions](https://github.com/evancz/Elm#elm)).

Then follow these steps to get the website running locally:

```bash
git clone https://github.com/elm-lang/elm-lang.org.git
cd elm-lang.org
git checkout stable
elm-package install
cabal configure
cabal build
./dist/build/run-elm-website/run-elm-website
```
If you get the following error message:

```bash
cabal: At least the following dependencies are missing:
```

...

You may need to run the following command 
```bash
cabal install --only-dependencies
```

Great! You should be set up with [elm-lang.org](http://elm-lang.org/) running at
[localhost:8000/](http://localhost:8000/).

You can run `cabal clean` to clear out all cached build information and start fresh.

### Project Structure

- `public/` &mdash; all of the .elm files used for the site. This makes up the
  majority of client-side code.  You can change/delete the existing files and
  add entirely new files. The changes, deletions, and additions will be served
  automatically.

- `resources/` &mdash; the various resources needed for Elm. This is where you
  put all of your non-Elm content, like images, videos, JavaScript code, etc.

- `server/` &mdash; the Haskell files responsible for serving everything from
  .elm files to images. Look here if you need to change how a particular
  resource is served or if you want to disable some of the sites features (such
  as the online editor).
