module Data.Package exposing (..)

import Dict exposing (Dict)
import Data.Version exposing (Version(..))


defaults : Dict String Version
defaults =
  Dict.fromList
    [ ( "elm/browser", Version 1 0 1 )
    , ( "elm/core", Version 1 0 2 )
    , ( "elm/html", Version 1 0 0 )
    , ( "elm/json", Version 1 1 3 )
    , ( "elm/time", Version 1 0 0 )
    , ( "elm/url", Version 1 0 0 )
    , ( "elm/virtual-dom", Version 1 0 2 )
    ]