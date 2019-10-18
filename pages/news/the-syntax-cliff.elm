import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Skeleton
import Center


main =
  Skeleton.news
    "The Syntax Cliff"
    "Teaching syntax with Elm 0.19.1"
    Skeleton.evan
    (Skeleton.Date 2019 10 21)
    [ Center.markdown "600px" intro
    , badImport
    , Center.markdown "600px" part1
    , missingCurlyBrace
    , Center.markdown "600px" part2
    , capitalNameError
    , Center.markdown "600px" part3
    ]


intro : String
intro = """

**When you start learning a programming language, how much time do you spend stuck on syntax errors?** Hours? Days? As someone who once spent a full day trying to debug a MapReduce build before learning that you need backslashes in multi-line bash expressions, I know it is a deeply discouraging amount of time. Lots of people have suffered through experiences like this with missing semi-colons and curly braces, but **how many people do not make it past these syntax errors?** How many people fall off the syntax cliff and give up on a language or just quit programming entirely?

![tightrope](/assets/blog/0.19.1/tightrope.png)

I started wondering how much of this problem comes down to error message quality. Could I get the compiler to a point where people feel like it is actually _helping_ them learn Elm syntax?

So with the release of Elm 0.19.1 today, I am excited to share the new and improved syntax error messages! My hope is that the new compiler feels more like a teacher, showing helpful and relevant examples when you get stuck. The remainder of this post highlights some of the messages that people are likely to see when learning Elm, so you can decide for yourself!

> **Note:** This is a patch release, so existing users should be able to just download [the new version](https://github.com/elm/compiler/releases/tag/0.19.1) and start using it with existing projects.


## Learning New Syntax

Say you are learning Elm and do not know how to `import` modules yet. Maybe you just try the JavaScript way to see if it works:

"""


badImport : Html msg
badImport =
  viewErrorMessage
    [ color keyword "import", text " * ", color keyword "from", text " 'set'\n\n"
    , color keyword "type alias", text " ", color def "Student", text " =\n"
    , text "    { firstName : String\n"
    , text "    , lastName : String\n"
    , text "    , completedAssignmentIds : Set Int\n"
    , text "    }\n\n"
    , color def "toFullName", text " : Student -> String\n"
    , color def "toFullName", text " student =\n"
    , text "  student.firstName ++ ", color dullYellow "\" \"", text " ++ student.lastName\n"
    ]
    [ color dullCyan "-- EXPECTING IMPORT NAME ------------------------------------------ src/Main.elm\n\n"
    , text "I was parsing an `import` until I got stuck here:\n\n"
    , text "1| import * from 'set'\n          ", color red "^"
    , text "\nI was expecting to see a module name next, like in these examples:\n\n"
    , text "    ", color cyan "import", text " Dict\n"
    , text "    ", color cyan "import", text " Maybe\n"
    , text "    ", color cyan "import", text " Html.Attributes ", color cyan "as", text " A\n"
    , text "    ", color cyan "import", text " Json.Decode ", color cyan "exposing", text " (..)\n\n"
    , text "Notice that the module names all start with capital letters. That is required!\n\n"
    , text "Read <https://elm-lang.org/0.19.1/imports> to learn more."
    ]


part1 : String
part1 = """

The new error messages points to the spot where it got stuck, but more importantly, it tries to help by (1) giving examples and (2) linking to a page that explains how imports work. It tries to help you learn!

This syntax error is pretty easy to detect though. What about harder cases?


## Missing Curly Braces

Have you ever forgotten a curly brace in JavaScript? Forget one little character and the error message shows up at the very end of the file. I still struggle when I get that message even after a decade of experience with JavaScript!

One of the major goals with the new parser was to improve this particular type of error message. Say you are defining a `Student` but forget the closing curly brace:

"""


missingCurlyBrace : Html msg
missingCurlyBrace =
  viewErrorMessage
    [ color keyword "import", text " Set ", color keyword "exposing", text " (..)\n\n"
    , color keyword "type alias", text " ", color def "Student", text " =\n"
    , text "    { firstName : String\n"
    , text "    , lastName : String\n"
    , text "    , completedAssignmentIds : Set Int\n\n\n"
    , color def "toFullName", text " : Student -> String\n"
    , color def "toFullName", text " student =\n"
    , text "  student.firstName ++ ", color dullYellow "\" \"", text " ++ student.lastName\n"
    ]
    [ color dullCyan "-- UNFINISHED RECORD TYPE ----------------------------------------- src/Main.elm\n\n"
    , text "I was partway through parsing a record type, but I got stuck here:\n\n"
    , text "4|     { firstName : String\n"
    , text "5|     , lastName : String\n"
    , text "6|     , completedAssignmentIds : Set Int\n"
    , text "                                         ", color red "^"
    , text "\nI was expecting to see a closing curly brace next. Try putting a ", color green "}", text " next and see\nif that helps?\n\n"
    , underline "Note", text ": I may be confused by indentation. For example, if you are trying to define\n"
    , text "a record type across multiple lines, I recommend using this format:\n\n"
    , text "    { name : String\n"
    , text "    , age : Int\n"
    , text "    , height : Float\n"
    , text "    }\n\n"
    , text "Notice that each line starts with some indentation. Usually two or four spaces.\n"
    , text "This is the stylistic convention in the Elm ecosystem."
    ]


part2 : String
part2 = """

The error message actually suggests a viable fix!

Elm has a rule that any definition must be defined on a fresh line. It cannot have any spaces in front of it. One benefit of this rule is that the compiler can always pinpoint the particular definition that contains a syntax error. No more errors at the end of the file!

Another example of using rules to get guarantees appears with variable naming.


## Variable Naming

Elm uses capitalization to differentiate certain kinds of names. Module names start with an upper-case letter, like `List` and `Http`, whereas function and argument names start with a lower-case letter, like `length` and `request`. These rules make it easier to scan through new code, but they can be surprising in the learning phase, especially if you are used to JavaScript or English.

So when someone is playing with the [examples](/examples) in the online editor, making a capitalization mistake will go something like this:

"""


capitalNameError : Html msg
capitalNameError =
  viewErrorMessage
    [ color keyword "import", text " Set ", color keyword "exposing", text " (..)\n\n"
    , color keyword "type alias", text " ", color def "Student", text " =\n"
    , text "    { firstName : String\n"
    , text "    , lastName : String\n"
    , text "    , completedAssignmentIds : Set Int\n"
    , text "    }\n\n"
    , color def "ToFullName", text " : Student -> String\n"
    , color def "ToFullName", text " student =\n"
    , text "    student.firstName ++ ", color dullYellow "\" \"", text " ++ student.lastName\n"
    ]
    [ color dullCyan "-- UNEXPECTED CAPITAL LETTER -------------------------------------- src/Main.elm"
    , text "\n\nDeclarations always start with a lower-case letter, so I am getting stuck here:\n\n"
    , text "3| ToFullName : Student -> String\n   ", color red "^"
    , text "\nTry a name like ", color green "toFullName", text " instead?\n\n"
    , underline "Note", text ": Here are a couple valid declarations for reference:\n\n"
    , text "    greet : String -> String\n"
    , text "    greet name =\n"
    , text "      ", color dullYellow "\"Hello \"", text " ++ name ++ ", color dullYellow "\"!\"\n\n"
    , text "    ", color cyan "type", text " User = Anonymous | LoggedIn String\n\n"
    , text "Notice that they always start with a lower-case letter. Capitalization matters!"
    ]


part3 : String
part3 = """

The new error message (1) suggests a fix and (2) gives examples to help teach the overall rules.

I encourage you to try to make naming errors in a language you are already familiar with. Can you imagine making that mistake when you started with the language? How is the error message? Would you know what it meant if you were just learning to program?


## Survivorship Bias

Trying to improve error messages seems like a worthwhile idea, so why is it uncommon for compilers to have syntax error messages like this? And why did it take so long for Elm to prioritize this project? I think part of the answer is [survivorship bias](https://en.wikipedia.org/wiki/Survivorship_bias).

Syntax errors are highly concentrated in the first weeks with a language, and people are particularly vulnerable in this time. **When a beginner asks themselves why something is hard, it is easy to think, "Because I am bad at it!"** And it is easy to spiral from there. "I heard it was hard. I was not super confident I could do it anyway. Maybe I just suck at this. And if this is what programming feels like, there is no chance I want to be doing this with my life!" People who fall off the cliff cannot share their perspective in meetups, online forums, conferences, etc. They quit! They are not in those places!

As for people who make it past the cliff, many do not shake off that initial confidence blow. They use the language, but not with enough confidence to think that their problems should be handled by a language designer. "Oh, that again. I will never learn!"

So language designers never really hear about this problem. I only understood its magnitude once `elm/error-message-catalog` got going. That repo solicits confusing error messages in hopes of finding ways to improve. I think projects like that legitimize the idea that "error messages should be better" such that I started hearing from a broader range of people. (Not just the very non-random sample of users that participate online!)

I personally think survivorship bias is a huge trap for language designers when it comes to prioritization. "Everyone is telling me to work on something else!" I find it really hard to put that aside even when I know "everyone" is actually a very particular sample, and I imagine it is only harder for language designers at the big firms with bosses prioritizing adoption by _existing_ programmers over everything else. "Is this language bringing developers to our cloud services? Is this VM bringing people to the default search provider in our browser? Is the reputation of this project making talent acquisition cheaper?" People who work on programming languages understand the implicit conditions of their employment, and it varies a lot by project.

Point is, I hope this work on syntax error messages will help make Elm more friendly and accessible, and I hope it will help make space for other language designers to prioritize this kind of project!


## Try it out

If you are interested in exploring the new syntax error messages with Elm 0.19.1, you can experiment with [examples](/examples) in our online editor or start working through the [The Official Guide](https://guide.elm-lang.org/).

I am sure there are ways the messages can be improved further, so I encourage you to share any confusing error messages in [`elm/error-message-catalog`](https://github.com/elm/error-message-catalog/issues) if you run into a tough one while learning!

I appreciate folks taking the time to give Elm a shot, and I hope it is fun to explore!


# Thank You

A lot of folks helped out testing Elm 0.19.1, but I want to specifically thank @jfmengels for stress testing the syntax error messages and @hrk for reading the 5800+ line syntax error file straight through! Both found a bunch of ways to improve the error messages even more.

I also want to thank the folks who have reported confusing error messages to [`elm/error-message-catalog`](https://github.com/elm/error-message-catalog/issues) over the years. It has been extremely helpful in inspiring and guiding projects to improve error message quality.

Thank you!

"""

{-

https://github.com/elm/error-message-catalog/issues/264 . json
https://github.com/elm/error-message-catalog/issues/261 . list brackets
https://github.com/elm/error-message-catalog/issues/255 ! record close curly indent
https://github.com/elm/error-message-catalog/issues/251 . missing else
https://github.com/elm/error-message-catalog/issues/248 ! minus sign
https://github.com/elm/error-message-catalog/issues/244 . REPL
https://github.com/elm/error-message-catalog/issues/241 . missing \\ in lambda
https://github.com/elm/error-message-catalog/issues/225 ! case indent
https://github.com/elm/error-message-catalog/issues/220 . REPL " in field name
https://github.com/elm/error-message-catalog/issues/196 . list close
https://github.com/elm/error-message-catalog/issues/195 ! missing else
https://github.com/elm/error-message-catalog/issues/168 ! extra comma
https://github.com/elm/error-message-catalog/issues/163 ! upper-case names
https://github.com/elm/error-message-catalog/issues/142 ! missing : in annotation
https://github.com/elm/error-message-catalog/issues/139 ! `as` keyword
https://github.com/elm/error-message-catalog/issues/136 ! module declarations
https://github.com/elm/error-message-catalog/issues/76  . `port` as variable name
https://github.com/elm/error-message-catalog/issues/62  . missing : in annotation
https://github.com/elm/error-message-catalog/issues/14  . else indentation

-}


viewErrorMessage : List (Html msg) -> List (Html msg) -> Html msg
viewErrorMessage code error =
  div
    [ style "overflow-x" "auto"
    , style "max-width" "84ch"
    , style "display" "block"
    , style "margin" "0 auto"
    ]
    [ div
        [ style "white-space" "pre"
        , style "width" "84ch"
        , style "font-family" "'Source Code Pro', monospace"
        ]
        [ div
            [ style "color" "black"
            , style "background-color" "#fcfcfe"
            , style "padding" "2ch"
            ]
            code
        , div
            [ style "color" "white"
            , style "background-color" "black"
            , style "padding" "2ch"
            ]
            error
        ]
    ]


underline : String -> Html msg
underline str =
  span [ style "text-decoration" "underline" ] [ text str ]


color : String -> String -> Html msg
color clr str =
  span [ style "color" clr ] [ text str ]


cyan : String
cyan =
  "rgb(20,240,240)"


dullCyan : String
dullCyan =
  "rgb(51,187,200)"


red : String
red =
  "rgb(252,57,31)"


dullYellow : String
dullYellow =
  "rgb(173,173,39)"


green : String
green =
  "rgb(49,231,34)"


purple : String
purple =
  "#ae81ff"


keyword : String
keyword =
  "#d800af"


def : String
def =
  "#7e1edf"
