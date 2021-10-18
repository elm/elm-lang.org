module Data.Http exposing
  ( Error(..)
  , onlyDecodedErrors
  , expectJson
  )


import Http
import Json.Decode as D


type Error x
  = ServerError Int x
  | DecodeError Int D.Error
  | HttpError Http.Error


onlyDecodedErrors : Error x -> Maybe x
onlyDecodedErrors err =
  case err of
    ServerError _ x -> Just x
    DecodeError _ _ -> Nothing
    HttpError _     -> Nothing


expectJson : (Result (Error x) a -> msg) -> D.Decoder x -> D.Decoder a -> Http.Expect msg
expectJson toMsg decodeErr decodeOk =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (HttpError <| Http.BadUrl url)

        Http.Timeout_ ->
          Err (HttpError Http.Timeout)

        Http.NetworkError_ ->
          Err (HttpError Http.NetworkError)

        Http.BadStatus_ metadata body ->
          case D.decodeString decodeErr body of
            Ok value ->
              Err (ServerError metadata.statusCode value)

            Err err ->
              Err (DecodeError metadata.statusCode err)

        Http.GoodStatus_ metadata body ->
          case D.decodeString decodeOk body of
            Ok value ->
              Ok value

            Err err ->
              Err (DecodeError metadata.statusCode err)