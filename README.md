elm-lang.org
============

All source files used to create [elm-lang.org](http://elm-lang.org/), the home-page of the Elm programming language.

### Structure

- `public/` -- all of the .elm files used for the site. This makes up the majority of client-side code.
- `server/` -- the two Haskell files responsible for serving everything from .elm files to images.
- `resources/` -- the various resources needed for Elm.

### Set up

Follow these steps to get this all running on your local machine:

##### Unix / Mac

- Fork or [download](https://github.com/evancz/elm-lang.org/downloads) this project.
- Run `bash compile.sh`. This will compile the server.
- Run `./Server` to start the server.

##### Windows

- Fork or [download](https://github.com/evancz/elm-lang.org/downloads) this project.
- Run `compile.bat`. This will compile the server and start it on your machine.

Great! You should be set up with [elm-lang.org](http://elm-lang.org/) running at [localhost:8000/](http://localhost:8000/).