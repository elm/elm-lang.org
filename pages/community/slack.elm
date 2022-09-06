
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import String

import Center
import Skeleton


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }



-- MODEL


type alias Model =
  { email : String
  , status : Status
  }


type Status
  = Unsent
  | Waiting
  | Sent Outcome
  | Error



-- INIT


init : () -> (Model, Cmd Msg)
init () =
  ( { email = "", status = Unsent }
  , Cmd.none
  )



-- UPDATE


type Msg
  = ChangeEmail String
  | InviteMe
  | GotOutcome (Result Http.Error Outcome)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeEmail email ->
      ( { email = email, status = Unsent }
      , Cmd.none
      )

    InviteMe ->
      ( model
      , Http.post
          { url = "https://worker.elm-lang.org/slack-invite?email=" ++ model.email
          , body = Http.emptyBody
          , expect = Http.expectJson GotOutcome decoder
          }
      )

    GotOutcome result ->
      case result of
        Ok outcome ->
          ( { model | status = Sent outcome }
          , Cmd.none
          )

        Err _ ->
          ( { model | status = Error }
          , Cmd.none
          )



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Elm Slack"
  , body =
      [ Skeleton.header Skeleton.Community
      , div [ style "flex" "1" ] [ viewContent model ]
      , Skeleton.footer
      ]
  }


viewContent : Model -> Html Msg
viewContent model =
  div (class "content" :: Center.styles "600px")
    [ h1 [] [ text "Elm Slack" ]
    , p []
        [ text "The way Slack works, you must be a member of a workspace before you can log in and start chatting. This is nice for companies using Slack internally because they can just add your email when you get hired, but we have to do this process a bit more manually:"
        ]
    , div [ style "display" "flex-box" ]
        [ input
            [ placeholder "Email"
            , value model.email
            , onInput ChangeEmail
            , disabled (isWaiting model.status)
            ]
            []
        , button
            [ onClick InviteMe
            , disabled (isWaiting model.status)
            ]
            [ text "Invite Me!"
            ]
        ]
    , p []
        [ text "After clicking "
        , code [] [ text "Invite Me" ]
        , text ", you should get an invitation email within a few minutes. From there, you can chat on "
        , a [ href "https://elmlang.slack.com" ] [ code [] [ text "https://elmlang.slack.com" ] ]
        , text " like any other Slack workspace you might use."
        ]
    ]


isWaiting : Status -> Bool
isWaiting status =
  case status of
    Unsent  -> False
    Waiting -> True
    Sent _  -> False
    Error   -> False



-- DECODER


type Outcome
  = InvinteSent
  | AlreadyInvited
  | AlreadyInTeam
  | InvalidEmail
  | ConfigError


decoder : D.Decoder Outcome
decoder =
  D.field "ok" D.bool
    |> D.andThen decoderHelp


decoderHelp : Bool -> D.Decoder Outcome
decoderHelp isOk =
  if isOk
  then D.succeed InvinteSent
  else D.map (errorToOutcome) (D.field "error" D.string)


errorToOutcome : String -> Outcome
errorToOutcome err =
  case err of
    "already_invited" -> AlreadyInvited
    "already_in_team" -> AlreadyInTeam
    "invalid_email"   -> InvalidEmail
    "invalid_auth"    -> ConfigError
    _                 -> ConfigError
