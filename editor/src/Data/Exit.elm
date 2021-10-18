module Data.Exit exposing (Exit(..), toString, toStringDetails, decoder)


import Json.Decode as JD
import Json.Encode as JE



type Exit
  = RequestBadPackageRegistry
  | RequestBadArtifacts String
  | RequestBodySizeTooLarge Int
  | RequestFormLargerThan32KB
  | RequestBodyDecodingFailed
  | RequestFormSizeTooLarge Int
  | RequestFormTooManyFields
  | RequestUnknownError String


toString : Exit -> String
toString exit =
  case exit of
    RequestBodySizeTooLarge _ ->
      "BodySizeTooLarge"

    RequestFormLargerThan32KB ->
      "FormLargerThan32KB"

    RequestBodyDecodingFailed ->
      "BodyDecodingFailed"

    RequestFormSizeTooLarge _ ->
      "FormSizeTooLarge"

    RequestFormTooManyFields ->
      "FormTooManyFields"

    RequestUnknownError _ ->
      "UnknownError"

    RequestBadPackageRegistry ->
      "BadPackageRegistry"

    RequestBadArtifacts _ ->
      "BadArtifacts"



toStringDetails : Exit -> String
toStringDetails exit =
  case exit of
    RequestBodySizeTooLarge size ->
      "{ size: " ++ String.fromInt size ++ " }"

    RequestFormLargerThan32KB ->
      ""

    RequestBodyDecodingFailed ->
      ""

    RequestFormSizeTooLarge size ->
      "{ size: " ++ String.fromInt size ++ " }"

    RequestFormTooManyFields ->
      ""

    RequestUnknownError message ->
      "{ message: " ++ message ++ " }"

    RequestBadPackageRegistry ->
      ""

    RequestBadArtifacts path ->
      "{ path: " ++ path ++ " }"



decoder : JD.Decoder Exit
decoder =
  let decodeExtras str =
        case str of
          "RequestBodySizeTooLarge" ->
            JD.map RequestBodySizeTooLarge (JD.field "size" JD.int)

          "RequestFormLargerThan32KB" ->
            JD.succeed RequestFormLargerThan32KB

          "RequestBodyDecodingFailed" ->
            JD.succeed RequestBodyDecodingFailed

          "RequestFormSizeTooLarge" ->
            JD.map RequestFormSizeTooLarge (JD.field "size" JD.int)

          "RequestFormTooManyFields" ->
            JD.succeed RequestFormTooManyFields

          "RequestUnknownError" ->
            JD.map RequestUnknownError (JD.field "message" JD.string)

          "RequestBadPackageRegistry" ->
            JD.succeed RequestBadPackageRegistry

          "RequestBadArtifacts" ->
            JD.map RequestBadArtifacts (JD.field "path" JD.string)

          _ ->
            JD.succeed (RequestUnknownError "Could not decode unknown error status")
  in
  JD.field "exit" JD.string
    |> JD.andThen decodeExtras