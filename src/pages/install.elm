import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Center
import TopBar


port title : String
port title =
  "Install Elm"


main =
  div []
    [ TopBar.topBar "install"
    , Center.markdown "600px" install
    ]



install = """

# Install

  * Mac &mdash; [installer](http://install.elm-lang.org/Elm-Platform-0.16.pkg)
  * Windows &mdash; [installer](http://install.elm-lang.org/Elm-Platform-0.16.exe)
  * Anywhere &mdash; [npm installer][npm] or [build from source][build]

[npm]: https://www.npmjs.com/package/elm
[build]: https://github.com/elm-lang/elm-platform

## Syntax Highlighting

  * [Atom](https://atom.io/packages/language-elm)
  * [Brackets](https://github.com/lepinay/elm-brackets)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [Light Table](https://github.com/rundis/elm-light)
  * [Sublime Text](https://github.com/deadfoxygrandpa/Elm.tmLanguage)
  * [Vim](https://github.com/lambdatoast/elm.vim)


## Help

If you are stuck, check to see if anyone has had [a similar issue][elm-platform].
If not, open a new issue or email [the list][group] or ask a question in the
[#elm IRC channel][irc].

[elm-platform]: https://github.com/elm-lang/elm-platform/issues
[group]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss
[irc]: http://webchat.freenode.net/?channels=elm

## Upgrade / Uninstall

To upgrade to a newer version of Elm, run the installer again. They displace
old executables so your machine is in a consistent state.

The Windows installer comes bundled with an uninstall option. To uninstall on
Mac, run [this script][uninstall]. If you built from source, delete everything
and change your PATH so that it no longer refers to those executables.

[uninstall]: https://github.com/elm-lang/elm-platform/blob/master/installers/mac/helper-scripts/uninstall.sh

"""
