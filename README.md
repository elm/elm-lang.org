# Elm Home Page

All of [elm-lang.org](http://elm-lang.org) is written in Elm. This repo is the client code (in Elm) and server code (in Haskell) that makes it all work.

You can use this to run [elm-lang.org/try](http://elm-lang.org/try) locally, but it is quite a challenge to set up.


## Build From Source

First get the Elm developer workflow setup by reading the [build from source instructions][bfs-readme], then running [this script][bfs] with `runhaskell BuildFromSource.hs 0.17`.

[bfs-readme]: https://github.com/elm-lang/elm-platform/blob/master/README.md
[bfs]: https://github.com/elm-lang/elm-platform/blob/master/installers/BuildFromSource.hs

Then in the `Elm-Platform/0.17/` directory, run these commands:

```bash
git clone https://github.com/elm-lang/elm-lang.org.git
cd elm-lang.org
git checkout master
cabal sandbox init --sandbox ../.cabal-sandbox
cabal install --only-dependencies
cabal configure
cabal build
./dist/build/run-elm-website/run-elm-website
```

Great! You should be set up with [elm-lang.org](http://elm-lang.org/) running at
[localhost:8000/](http://localhost:8000/).

You can run `cabal clean` to clear out all cached build information and start fresh.
