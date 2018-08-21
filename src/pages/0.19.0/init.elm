import Skeleton


main =
  Skeleton.hint "Creating an Elm project" """

All projects begin with an `elm.json` file and a `src/Main.elm` file.

As you work through [the official guide](https://guide.elm-lang.org/), you can put the code examples in your `src/Main.elm` file.


## Compiling your Code

Run `elm reactor` in your project. Now you can go to [`http://localhost:8000`](http://localhost:8000) and browse through all the files in your project. If you navigate to `.elm` files, it will compile them for you!


## Growing your Code

Many folks get anxious about their project structure. “If I get it wrong, I am doomed!” This anxiety makes sense in languages where refactoring is risky, but Elm is not one of those languages!

So we recommend that newcomers staying in one file until you get into the 600 to 1000 range. Push out of your comfort zone. Having the experience of being fine in large files will help you understand the boundaries in Elm, rather than just defaulting to the boundaries you learned in another language.

The talk [The Life of a File](https://youtu.be/XpDsk374LDE) gets into this a lot more. The advice about building modules around a specific custom type is particularly important! You will see that emphasized a lot as you work through [the official guide](https://guide.elm-lang.org/).


## Testing your Code

Elm will catch a bunch of errors statically, but it also has a really great testing package called [`elm-explorations/test`](https://github.com/elm-explorations/test) that can help you catch the rest.

This can be particularly helpful for teams working on a large codebase. When you are editing code you have never seen before, it can be helpful to capture additional details and constraints in tests!

"""