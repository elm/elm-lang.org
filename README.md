# elm-lang.org: a template for creating websites in Elm

This project contains all of the source files used to create [elm-lang.org](http://elm-lang.org/),
the home-page of the Elm programming language.

The project provides a general structure for creating your own Elm website, mixing Elm, images,
videos, HTML/CSS/JS, and whatever else you need.

This project includes a Haskell server that determines how to serve each kind of file, so you
only need to think about the content.

### Set up

First make sure that you have the Elm compiler installed
([directions](https://github.com/evancz/Elm#elm)).

Then follow these steps to get this all running on your local machine:

###### Unix / Mac

- Fork this project.
- Run `bash compile.sh`. This will compile the server.
- Run `./ElmServer` to start the server.

###### Windows

- Fork this project.
- Run `compile.bat`. This will compile the server and start it on your machine.

Great! You should be set up with [elm-lang.org](http://elm-lang.org/) running at [localhost:8000/](http://localhost:8000/).

### Project Structure

- `public/` &mdash; all of the .elm files used for the site. This makes up the majority of client-side code.
  You can change/delete the existing files and add entirely new files. The changes, deletions, and additions will
  be served automatically.
- `resources/` &mdash; the various resources needed for Elm. This is where you put all of your non-Elm content,
  like images, videos, JavaScript code, etc.
- `server/` &mdash; the Haskell files responsible for serving everything from .elm files to images. Look here
  if you need to change how a particular resource is served or if you want to disable some of the sites
  features (such as the online editor).

