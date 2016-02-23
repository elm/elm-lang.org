# Contributing

This section will go over how you can contribute to the Elm's libraries and core tools.

There are a bunch of projects in [the elm-lang
organization](http://github.com/elm-lang), including the compiler, REPL, server,
package manager, debugger, public library, and this website.

We have found that a good way to make contributions is to hang out on the
[mailing list][list] to learn about the ongoing challenges. Becoming a part of
this discussion will make it much clearer how you can effectively help the
community or a specific project based on your skills and interests.

## Elm Platform

The [Elm Platform](https://github.com/elm-lang/elm-platform) is a bundle of Elm tools,
, usable through the elm executable. This project can also be used to build the tools 
for development.

For example, if you want to contribute to the [elm-package](https://github.com/elm-lang/elm-package) project
you can use [Elm Platform](https://github.com/elm-lang/elm-platform) as the tool to bundle and
test the changes you have made.

Here are some steps:

```bash
git clone git@github.com:elm-lang/elm-platform.git
cd elm-platform
git checkout master
runhaskell installers/BuildFromSource.hs master
cd Elm-Platform/master
```

If you look in this directory, there is a good chance you will see the [elm-package](https://github.com/elm-lang/elm-package) tool
already there. **Remove it**.

Fork [elm-package](https://github.com/elm-lang/elm-package) and clone in into the Elm-Package/master
directory.

```bash
git clone https://github.com/<your github username>/elm-package.git
cd elm-package
git checkout master
cabal sandbox init --sandbox ..
cabal configure
cabal install --only-dependencies
cabal build
./dist/build/elm-package/elm-package
```

You can do this for any of the other projects that use Elm-Platform ([`elm-compiler`][compiler], [`elm-make`][make], [`elm-reactor`][reactor], [`elm-repl`][repl], [`elm-package`][package]).

[compiler]: https://github.com/elm-lang/elm-compiler
[make]: https://github.com/elm-lang/elm-make
[reactor]: https://github.com/elm-lang/elm-reactor
[repl]: https://github.com/elm-lang/elm-repl
[package]: https://github.com/elm-lang/elm-package
