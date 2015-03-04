import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import Http
import Json.Decode exposing (..)
import String
import Window

-- VIEW

view : (Int,Int) -> Field.Content -> Maybe String -> Element
view (w,h) searchContent imgSrc =
  let currentImage =
        case imgSrc of
          Just src ->
              fittedImage w (h-100) src

          Nothing ->
              container w (h-100) middle (image 16 16 "/waiting.gif")

      currentField =
        Field.field
            Field.defaultStyle
            (Signal.send tagChan)
            "Flickr Instant Search"
            searchContent
  in
      flow down
        [ container w 100 middle currentField
        , currentImage
        ]


-- SIGNALS

{-| Pass in the current dimensions and image. All inputs are
signals and will update automatically.
-}
main : Varying Element
main =
  Varying.map3 view
      Window.dimensions
      (Signal.subscribe tagChan)
      results


results : Varying (Maybe String)
results =
  Signal.subscribe tagChan
    |> Varying.map .string
    |> Signal.dropRepeats
    |> Varying.map toPhotoRequest
    |> Http.send
    |> Varying.map toSizeRequest
    |> Http.send
    |> Varying.map2 toPhotoSource Window.dimensions


tagChan : Signal.Channel Field.Content
tagChan =
  Signal.channel Field.noContent


-- JSON

type alias Photo = { id:String, title:String }

photoList : Decoder (List Photo)
photoList =
  at ["photos","photo"] <| list <|
      object2 Photo
        ("id" := string)
        ("title" := string)


type alias Size = { source:String, width:Int, height:Int }

sizeList : Decoder (List Size)
sizeList =
  let number =
        oneOf [ int, customDecoder string String.toInt ]
  in
      at ["sizes","size"] <| list <|
          object3 Size
            ("source" := string)
            ("width" := number)
            ("height" := number)


decodeResponse : Decoder a -> Http.Response String -> Result String a
decodeResponse decoder response =
    case response of
      Http.Success str ->
          decodeString decoder str

      _ -> Err (toString response)


--  REQUEST LOGIC

{-| The standard parts of a Flickr API request. -}
flickrRequest : String -> String
flickrRequest args =
    "https://api.flickr.com/services/rest/?format=json"
    ++ "&nojsoncallback=1&api_key=9be5b08cd8168fa82d136aa55f1fdb3c"
    ++ args


{-| From a tag, create a GET request for relevant photos. -}
toPhotoRequest : String -> Http.Request String
toPhotoRequest tag =
  let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
  in
      Http.get (if tag == "" then "" else flickrRequest args ++ tag)


{-| From a list of photos, just take the first one you see and create a GET
request for the sizes available.
-}
toSizeRequest : Http.Response String -> Http.Request String
toSizeRequest json =
    case decodeResponse photoList json of
      Err _ ->
        Http.get ""

      Ok photos ->
        case photos of
          photo :: _ ->
            Http.get (flickrRequest "&method=flickr.photos.getSizes&photo_id=" ++ photo.id)

          [] ->
            Http.get ""


{-| From a list of photo sizes, choose the one closest to current window size. -}
toPhotoSource : (Int,Int) -> Http.Response String -> Maybe String
toPhotoSource (width,height) json =
    case decodeResponse sizeList json of
      Err msg ->
        Nothing

      Ok sizes ->
        let sizeRating =
              if width > height
                  then (\size -> abs (width - size.width))
                  else (\size -> abs (height - size.height))
        in
            case List.sortBy sizeRating sizes of
              size :: _ -> Just size.source
              [] -> Nothing
