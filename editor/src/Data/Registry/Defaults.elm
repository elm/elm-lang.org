module Data.Registry.Defaults exposing
  ( direct, indirect, locked, popular )

import Data.Registry.Package as Package
import Data.Version as V


direct : List Package.Package
direct =
  [ Package.Package "elm" "browser" (V.Version 1 0 2)
  , Package.Package "elm" "core" (V.Version 1 0 5)
  , Package.Package "elm" "html" (V.Version 1 0 0)
  ]


indirect : List Package.Package
indirect =
  [ Package.Package "elm" "json" (V.Version 1 1 3)
  , Package.Package "elm" "time" (V.Version 1 0 0)
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

