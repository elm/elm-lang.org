import Graphics.Input (Input, input)
import Graphics.Input.Field as Field
import Http
import JavaScript.Experimental as JS
import Json
import Window

----  Core Logic  ----

{-| Asynchronously get a photo with a given tag. Makes two
asynchronous HTTP requests to Flickr, resulting in the URL
of an image.
-}
getSources : Signal String -> Signal (Maybe String)
getSources tag = let photos = Http.send (lift getTag tag)
                     sizes  = Http.send (lift getOneFrom photos)
                 in  lift sizesToSource sizes

{-| Create an input for tags -}
tag : Input Field.Content
tag = input Field.noContent

{-| Put our text input and images together. Takes in the
dimensions of the browser and an image. Results in a search
box and large image result that fills the screen.
-}
scene : (Int,Int) -> Field.Content -> Maybe String -> Element
scene (w,h) searchContent imgSrc =
    flow down
      [ container w 100 middle <|
          Field.field Field.defaultStyle tag.handle id "Flickr Instant Search" searchContent
      , case imgSrc of
          Just src -> fittedImage w (h-100) src
          Nothing -> container w (h-100) middle (image 16 16 "/waiting.gif")
      ]

{-| Pass in the current dimensions and image. All inputs are
signals and will update automatically.
-}
main : Signal Element
main = scene <~ Window.dimensions
              ~ tag.signal
              ~ getSources (.string <~ dropRepeats tag.signal)


----  Helper Functions  ----

{-| The standard parts of a Flickr API request. -}
flickrRequest : String -> String
flickrRequest args =
  "http://api.flickr.com/services/rest/?format=json" ++
  "&nojsoncallback=1&api_key=9be5b08cd8168fa82d136aa55f1fdb3c" ++ args


{-| Turn a tag into an HTTP GET request. -}
getTag : String -> Http.Request String
getTag tag =
    let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
    in  Http.get (if tag == "" then "" else flickrRequest args ++ tag)

toJson : Http.Response String -> Maybe Json.Value
toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing

{-| Take a list of photos and choose one, resulting in a request. -}
getOneFrom : Http.Response String -> Http.Request String
getOneFrom photoList =
    case toJson photoList of
      Nothing -> Http.get ""
      Just json ->
          let photoRecord = JS.toRecord <| JS.fromJson json
          in  case photoRecord.photos.photo of
                h::_ -> Http.get (flickrRequest "&method=flickr.photos.getSizes&photo_id=" ++ h.id)
                []   -> Http.get ""

                        
{-| Take some size options and choose one, resulting in a URL. -}
sizesToSource : Http.Response String -> Maybe String
sizesToSource sizeOptions =
    case toJson sizeOptions of
      Nothing   -> Nothing
      Just json ->
          let sizesRecord = JS.toRecord <| JS.fromJson json
              sizes = sizesRecord.sizes.size
          in  case reverse sizes of
                _ :: _ :: _ :: _ :: _ :: size :: _ -> Just size.source
                size :: _ -> Just size.source
                _ -> Nothing
