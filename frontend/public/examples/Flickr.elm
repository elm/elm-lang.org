import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import JavaScript.Decode as JS exposing ((:=))
import Promise exposing (..)
import String
import Window


-- VIEW

view : Int -> String -> String -> Html
view h string imgUrl =
  div [ style (imgStyle h imgUrl) ]
    [ input'
        [ placeholder "Flickr Query"
        , Attr.value string
        , on "input" targetValue (Stream.message query.address)
        , style myStyle
        ]
        []
    ]


myStyle : List (String, String)
myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


imgStyle : Int -> String -> List (String, String)
imgStyle h src =
    [ ("background-image", "url('" ++ src ++ "')")
    , ("background-repeat", "no-repeat")
    , ("background-attachment", "fixed")
    , ("background-position", "center")
    , ("width", "100%")
    , ("height", toString h ++ "px")
    ]


-- WIRING

main : Varying Html
main =
  Varying.map3 view
    Window.height
    (Stream.toVarying "" query.stream)
    (Stream.toVarying "waiting.gif" <| Stream.filterMap Result.toMaybe results)


loopback results : Stream (Result Http.Error String)
loopback results <-
  Stream.sample getImage Window.dimensions query.stream


loopback query : Stream.Mailbox String


getImage : (Int,Int) -> String -> Promise Http.Error String
getImage dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
      Http.get photoList (flickr "search" searchArgs)
        `andThen`
            selectPhoto
        `andThen` \photo ->
            Http.get sizeList (flickr "getSizes" [ ("photo_id", photo.id) ])
        `andThen`
            pickSize dimensions


-- JSON DECODERS

type alias Photo =
    { id : String
    , title : String
    }


type alias Size =
    { source : String
    , width : Int
    , height : Int
    }


photoList : JS.Decoder (List Photo)
photoList =
  JS.at ["photos","photo"] <| JS.list <|
      JS.object2 Photo
        ("id" := JS.string)
        ("title" := JS.string)


sizeList : JS.Decoder (List Size)
sizeList =
  let number =
        JS.oneOf [ JS.int, JS.customDecoder JS.string String.toInt ]
  in
      JS.at ["sizes","size"] <| JS.list <|
          JS.object3 Size
            ("source" := JS.string)
            ("width" := number)
            ("height" := number)


--  FLICKR URLS

flickr : String -> List (String, String) -> String
flickr method args =
  Http.url "https://api.flickr.com/services/rest/" <|
    [ ("format", "json")
    , ("nojsoncallback", "1")
    , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
    , ("method", "flickr.photos." ++ method)
    ] ++ args


-- HANDLE RESPONSES

selectPhoto : List Photo -> Promise Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] ->
      fail (Http.UnexpectedPayload "expecting 1 or more photos from Flickr")


pickSize : (Int,Int) -> List Size -> Promise Http.Error String
pickSize (width,height) sizes =
  let sizeRating size =
        let penalty =
              if size.width > width || size.height > height then 400 else 0
        in
            abs (width - size.width) + abs (height - size.height) + penalty
  in
      case List.sortBy sizeRating sizes of
        size :: _ -> succeed size.source
        [] ->
          fail (Http.UnexpectedPayload "expecting 1 or more image sizes to choose from")
