import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import Skeleton


main =
  Skeleton.skeleton "install"
    [ Center.markdown "600px" install
    ]



install = """

# Install

  * Mac &mdash; [installer](http://install.elm-lang.org/Elm-Platform-0.17.1.pkg)
  * Windows &mdash; [installer](http://install.elm-lang.org/Elm-Platform-0.17.1.exe)
  * Anywhere &mdash; [npm installer][npm] or [build from source][build]

Afterwards, visit the [get started page][get-started].

[npm]: https://www.npmjs.com/package/elm
[build]: https://github.com/elm-lang/elm-platform
[get-started]: /get-started

## Syntax Highlighting

  * [Atom](https://atom.io/packages/language-elm)
  * [Brackets](https://github.com/lepinay/elm-brackets)
  * [Emacs](https://github.com/jcollard/elm-mode)
  * [Light Table](https://github.com/rundis/elm-light)
  * [Sublime Text](https://packagecontrol.io/packages/Elm%20Language%20Support)
  * [Vim](https://github.com/lambdatoast/elm.vim)
  * [VS Code](https://github.com/sbrink/vscode-elm)
  * [IntelliJ](https://github.com/durkiewicz/elm-plugin)


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
