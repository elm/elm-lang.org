import Html exposing (..)
import Html.Attributes exposing (..)

import Center
import Skeleton


main =
  Skeleton.skeleton "Elm - News" Skeleton.News
    [ div (Center.styles "600px")
        [ h1 [] [ text "News" ]
        , p []
            [ text "This page focuses on news about the compiler and core libraries. High quality compiler work takes time, so it may be a number of years between releases. People who stick with Elm tend to like that this approach (1) creates a stable foundation for package and application authors and (2) focuses more capacity on "
            , a [ href "https://github.com/elm/compiler/blob/master/roadmap.md" ] [ text "long range projects" ]
            , text ". This prioritization can be surprising for people used to "
            , a [ href "https://discourse.elm-lang.org/t/costs-funding-in-open-source-languages/5722" ] [ text "TAC funded languages" ]
            , text " which tend to have much larger bugdets and different priorities. So I hope you have a good experience with Elm, even if you ultimately find a different language that works better for you!"
            ]
        , p []
            [ text "For more frequent news, check out "
            , a [ href "https://twitter.com/elmlang" ] [ text "@elmlang" ]
            , text " and "
            , a [ href "/community" ] [ text "the community forums" ]
            , text " to hear community members share their packages, tooling, editors, blogs, projects, etc."
            ]
        , h2 [] [ text "Announcements" ]
        , ul [] (List.map viewNews news)
        , h2 [] [ text "Releases" ]
        , table [ class "releases" ]
            [ tbody [] (List.map viewRelease releases)
            ]
        , h2 [] [ text "Conference Videos" ]
        , ul [] (List.map viewVideo videos)
        ]
    ]



-- NEWS


type alias News =
  { name : String
  , href : String
  }


viewNews : News -> Html msg
viewNews article =
  li [] [ a [ href article.href ] [ text article.name ] ]


news : List News
news =
  [ News "The Syntax Cliff" "/news/the-syntax-cliff"
  , News "Towards a Homegrown Programming Language" "/news/elm-and-bekk"
  , News "Working with Files" "/news/working-with-files"
  , News "Small Assets without the Headache" "/news/small-assets-without-the-headache"
  , News "Google Summer of Code 2017" "/news/google-summer-of-code-2017"
  , News "The Perfect Bug Report" "/news/the-perfect-bug-report"
  , News "Blazing Fast HTML, Round Two" "/news/blazing-fast-html-round-two"
  , News "How to Use Elm at Work" "/news/how-to-use-elm-at-work"
  , News "A Farewell to FRP" "/news/farewell-to-frp"
  , News "New Adventures for Elm" "/news/new-adventures-for-elm"
  , News "Compilers as Assistants" "/news/compilers-as-assistants"
  , News "Compiler Errors for Humans" "/news/compiler-errors-for-humans"
  , News "Time Travel made Easy" "/news/time-travel-made-easy"
  , News "Blazing Fast HTML" "/news/blazing-fast-html"
--  , News "Elm&rsquo;s Time Traveling Debugger" "http://debug.elm-lang.org"
  , News "Elm package manager" "/news/package-manager"
  , News "Elm REPL" "/news/repl"
  , News "Hot-swapping in Elm" "/news/interactive-programming"
  , News "Concepts behind the Elm Logo" "https://prezi.com/oqd48bv5ef0s/tangrams-logo/"
--  , News "Elm in VentureBeat" "http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/"
  , News "Elm and Prezi" "/news/elm-and-prezi"
--  , News "Escape from Callback Hell" "/news/escape-from-callback-hell"
--  , News "Making Pong" "/news/making-pong"
  ]



-- RELEASES


type alias Release =
  { version : String
  , href : String
  , summary : String
  , date : String
  }


viewRelease : Release -> Html msg
viewRelease release =
  tr []
    [ td [] [ a [ href release.href ] [ text release.version ] ]
    , td [] [ text release.summary ]
    , td [] [ text release.date ]
    ]


releases : List Release
releases =
  [ Release "0.19.1" "/news/the-syntax-cliff" "Friendly syntax hints, faster builds" "Oct 2019"
  , Release "0.19" "/news/small-assets-without-the-headache" "Smaller assets, faster builds" "Aug 2018"
  , Release "0.18" "/news/the-perfect-bug-report" "New debugger with import/export" "Nov"
  , Release "0.17" "/news/farewell-to-frp" "Add subscriptions, remove signals" "May 2016"
  , Release "0.16" "/news/compilers-as-assistants" "Even better error messages!" "Nov"
  , Release "0.15.1" "/news/compiler-errors-for-humans" "Dramatically improved error messages" "Jun"
  , Release "0.15" "/news/0.15" "Tasks, better HTTP library" "Apr"
  , Release "0.14.1" "https://groups.google.com/forum/#!topic/elm-announce/6zRwjN68Ap0" "HTML through main" "Jan 2015"
  , Release "0.14" "/news/0.14" "Package manager, parallel builds, JSON" "Dec"
  , Release "0.13" "/news/0.13" "Debugging with elm-reactor" "Sep"
  , Release "0.12.3" "/news/0.12.3" "3D rendering with WebGL" "May"
  , Release "0.12.1" "/news/0.12.1" "Fast Immutable Arrays" "May"
  , Release "0.12" "/news/0.12" "Interactive UI Elements" "Mar"
  , Release "0.11" "/news/0.11" "Drastically improved FFI with ports" "Jan 2014"
  , Release "0.10.1" "/news/0.10.1" "Package manager integration" "Dec"
  , Release "0.10" "/news/0.10" "Strings, Colors, Operators" "Oct"
  , Release "0.9" "/news/0.9" "Fast and reliable type inference" "Aug"
  , Release "0.8" "/news/0.8" "HTML/JS integration" "May"
  , Release "0.7.1" "/news/0.7.1" "Libraries for touch, either, and keyboard" "Feb"
  , Release "0.7" "/news/0.7" "Extensible records" "Jan 2013"
  , Release "0.6" "/news/0.6" "Whitespace sensitivity" "Dec"
  , Release "0.5" "/news/0.5" "Libraries for dictionaries, sets, and automata" "Oct"
  , Release "0.4" "/news/0.4" "Markdown" "Sep"
  , Release "0.3.6" "https://web.archive.org/web/20130810052854/http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/" "JSON support" "Aug"
  , Release "0.3.5" "https://web.archive.org/web/20131010072233/http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/" "JavaScript FFI" "Jun"
  , Release "0.3" "https://web.archive.org/web/20131010015034/http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/" "Modules" "Jun"
  , Release "0.1" "https://www.reddit.com/r/haskell/comments/rkyoa/my_thesis_is_finally_complete_elm_concurrent_frp/" "Initial Release" "Apr 2012"
  ]



-- VIDEOS


type alias Video =
  { title : String
  , href : String
  , year : Int
  , summary : String
  }


viewVideo : Video -> Html msg
viewVideo video =
  li []
    [ p []
        [ a [ href video.href ] [ text video.title ]
        , text " – "
        , text (String.fromInt video.year)
        , br [] []
        , text video.summary
        ]
    ]


videos : List Video
videos =
  [ Video "Let’s be Mainstream!" "http://www.elmbark.com/2016/03/16/mainstream-elm-user-focused-design" 2015
      "“If functional programming is so great, why is it still niche? We have a product that can practically eliminate runtime errors, make refactoring much easier, lighten the testing burden, all while being quite delightful to use. What’s the hold up?”"
  , Video "What is Success?" "https://youtu.be/uGlzRt-FYto" 2018
      "Is it GitHub stars? Maximizing package downloads? Weekly blog posts? Adding ever more features? This talk gets into the implicit values that people bring to Elm, and tries to outline what is important in the Elm community."
  , Video "Code is the Easy Part" "https://youtu.be/DSjbTC-hvqQ" 2016
      "People often think that contributing to an open-source project is strictly about adding code and adding features. This creates some really unhealthy incentives and interactions. This talk emphasizes the importance of personal relationships, helping each other, and community participation as the foundation of a fruitful collaboration."
  , Video "The Life of a File" "https://youtu.be/XpDsk374LDE" 2017
      "Many folks get anxious about their project structure. “If I get it wrong, I am doomed!” This talk outlines the recommended approach for growing a large codebase. With the compiler making refactors easy, it is not as hard as you might think!"
  , Video "Accidentally Concurrent" "https://youtu.be/DfLvDFxcAIA" 2015
      "This talk examines references, objects, and reactivity in terms of concurrency. This reframing is a useful way of understanding the “accidental complexity” in your code base."
  , Video "Controlling Time and Space" "https://youtu.be/Agu6jipKfYw" 2015
      "Categorizes the many formulations of FRP, showing how they relate to Elm and what benefits you get from doing it the Elm way."
  , Video "Functional Reactive Programming in Elm" "http://www.infoq.com/presentations/elm-reactive-programming" 2013
      "Teaches the basics of graphics and FRP in Elm and builds up to implementing a basic Mario game, which is pretty fun to watch."
  , Video "Elm: Making the Web Functional" "http://www.infoq.com/presentations/Elm" 2012
      "First conference talk ever. Covers the basics of graphics and FRP before Elm even had its record system!"
  ]

-- [mlocjs]: http://www.ustream.tv/recorded/29330499
