port module Data.Analytics exposing ( gotJsError, reportError )


import Dict exposing (Dict)
import Data.Registry.Package as Pkg
import Data.Exit as Exit
import Json.Decode as JD
import Json.Encode as JE
import Http
import Time
import Data.Time
import Data.Frame as Frame
import DateFormat as F
import Set
import Constant


-- API


port gotJsError : (String -> msg) -> Sub msg


reportError : (Result Http.Error String -> msg) -> String -> Cmd msg
reportError onResult errMsg =
  Http.post
    { url = Constant.server ++ "/api/analytics/new-ui-error"
    , body = Http.jsonBody (JE.string errMsg)
    , expect = Http.expectJson onResult JD.string
    }


