import Graphics.Input as Input
import Http
import JavaScript.Experimental as JS
import Json
import Window

{------------------------  Core Logic  ------------------------}

-- This is the core logic of the Elm program described here:
--     http://elm-lang.org/learn/Escape-from-Callback-Hell.elm


-- Asynchronously get a photo with a given tag. Makes two
-- asynchronous HTTP requests to Flickr, resulting in
-- the URL of an image.

getSources : Signal String -> Signal (Maybe String)
getSources tag = let photos = Http.send (getTag <~ tag)
                     sizes  = Http.send (getOneFrom <~ photos)
                 in  sizesToSource <~ sizes

-- Create a text input box and a signal of tags, as seen in
-- "Escape from Callback Hell".

(tagInput, tags) = Input.field "Flickr Instant Search"


-- Put our text input and images together. Takes in the
-- dimensions of the browser and an image. Results in a
-- Search box and large image result that fills the screen.

scene : (Int,Int) -> Element -> Maybe String -> Element
scene (w,h) tagInput imgSrc =
    flow down
      [ container w 100 middle tagInput,
        case imgSrc of
          Just src -> fittedImage w (h-100) src
          Nothing -> container w (h-100) middle (image 16 16 "/waiting.gif")
      ]


-- Pass in the current dimensions and image. All inputs are
-- signals and will update automatically.

main = scene <~ Window.dimensions
              ~ tagInput
              ~ getSources (dropRepeats tags)





{---------------------  Helper Functions  ---------------------}

-- These are the functions that are needed in any
-- implementation, regardless of how you make your HTTP
-- requests and whether you are using JavaScript or Elm.
-- See "Escape from Callback Hell" for a more detailed
-- description of these functions.


-- The standard parts of a Flickr API request.
flickrRequest args =
  "http://api.flickr.com/services/rest/?format=json" ++
  "&nojsoncallback=1&api_key=256663858aa10e52a838a58b7866d858" ++ args


-- Turn a tag into an HTTP GET request.
getTag : String -> Http.Request String
getTag tag =
    let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
    in  Http.get (if tag == "" then "" else flickrRequest args ++ tag)

toJson response =
    case response of
      Http.Success str -> Json.fromString str
      _ -> Nothing

-- Take a list of photos and choose one, resulting in a request.
getOneFrom photoList =
    case toJson photoList of
      Nothing -> Http.get ""
      Just json ->
          let photoRecord = JS.toRecord <| Json.toJSObject json
          in  case photoRecord.photos.photo of
                h::_ -> Http.get (flickrRequest "&method=flickr.photos.getSizes&photo_id=" ++ h.id)
                []   -> Http.get ""

                        
-- Take some size options and choose one, resulting in a URL.
sizesToSource : Http.Response String -> Maybe String
sizesToSource sizeOptions =
    case toJson sizeOptions of
      Nothing   -> Nothing
      Just json ->
          let sizesRecord = JS.toRecord <| Json.toJSObject json
              sizes = sizesRecord.sizes.size
          in  case reverse sizes of
                _ :: _ :: _ :: _ :: _ :: size :: _ -> Just size.source
                size :: _ -> Just size.source
                _ -> Nothing
