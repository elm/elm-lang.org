module Data.Registry.Defaults exposing
  ( direct, indirect, locked, popular )

import Data.Registry.Package as Package
import Data.Version as V


direct : List Package.Package
direct =
  [ Package.Package "elm" "browser" (V.Version 1 0 2)
  , Package.Package "elm" "core" (V.Version 1 0 5)
  , Package.Package "elm" "html" (V.Version 1 0 0)
  , Package.Package "elm" "file" (V.Version 1 0 5)
  , Package.Package "elm" "http" (V.Version 2 0 0)
  , Package.Package "elm" "json" (V.Version 1 1 3)
  , Package.Package "elm" "random" (V.Version 1 0 0)
  , Package.Package "elm" "svg" (V.Version 1 0 1)
  , Package.Package "elm" "time" (V.Version 1 0 0)
  , Package.Package "elm-explorations" "linear-algebra" (V.Version 1 0 3)
  , Package.Package "elm-explorations" "webgl" (V.Version 1 1 0)
  , Package.Package "evancz" "elm-playground" (V.Version 1 0 2)
  ]


indirect : List Package.Package
indirect =
  [ Package.Package "elm" "bytes" (V.Version 1 0 8)
  , Package.Package "elm" "url" (V.Version 1 0 0)
  , Package.Package "elm" "virtual-dom" (V.Version 1 0 2)
  ]


locked : List Package.Key
locked =
  [ ( "elm", "browser" )
  , ( "elm", "core" )
  ]


popular : List Package.Key
popular =
  [ ( "elm", "http" )
  , ( "elm", "html" )
  , ( "elm", "random" )
  , ( "elm", "time" )
  , ( "elm", "file" )
  , ( "elm", "json" )
  , ( "elm", "svg" )
  , ( "evancz", "elm-playground" )
  , ( "elm-explorations", "webgl" )
  , ( "w0rm", "elm-physics")
  , ( "rtfeldman", "elm-css" )
  , ( "mdgriffith", "elm-ui" )
  ]

