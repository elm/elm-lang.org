
import HTTP
import JSON

{------------------------  Core Logic  ------------------------}

-- This is the core logic of the Elm program. You end up writing
-- way less code to create the entire application. Imagine
-- creating the same functionality and presentation with HTML,
-- CSS, and JavaScript.


-- Asynchronously get a photo with a given tag. Makes two
-- asynchronous HTTP requests to Flickr, resulting in
-- the URL of an image.

getPhotos tags =
  let photoList  = send (lift requestTag tags) in
  let photoSizes = send (lift requestOneFrom photoList) in
      lift sizesToPhoto photoSizes


-- Create a text input box and a signal of tags, as seen in
-- "Escape from Callback Hell".

(tagInput, tags) = Input.textField "Flickr Instant Search"


-- Put our text input and images together. Takes in the
-- dimensions of the browser and an image. Results in a
-- Search box and large image result that fills the screen.

scene (w,h) img = flow down [ container w 60 middle tagInput
                            , container w (h - 100) middle img ]


-- Pass in the current dimensions and image. Both inputs are
-- signals so they will update automatically.

main = lift2 scene Window.dimensions
                   (images (getPhotos (dropRepeats tags)))






{---------------------  Helper Functions  ---------------------}

-- These are the functions that are needed in any
-- implementation, regardless of how you make your HTTP
-- requests and whether you are using JavaScript or Elm.
-- See "Escape from Callback Hell" for a more detailed
-- description of these functions.


-- The standard parts of a Flickr API request.
flickrRequest =
  "http://api.flickr.com/services/rest/?format=json" ++
  "&nojsoncallback=1&api_key=66c61b93c4723c7c3a3c519728eac252"


-- Extract a JSON object from a HTTP response.
extract response =
  case response of
  { Success str -> JSON.fromString str
  ; _ -> empty }


-- Turn a tag into a request.
requestTag tag =
  if tag == "" then get "" else
  get (concat [ flickrRequest
              , "&method=flickr.photos.search"
              , "&sort=random"
              , "&tags=", tag
              , "&per_page=10" ])


-- Take a list of photos and choose one, resulting in a request.
requestOneFrom photoList =
  let { getPhotoID json =
          case findArray "photo" (findObject "photos" json) of
          { (JsonObject hd) : tl -> findString "id" hd ; _ -> "" }
      ; requestSizes id = if id == "" then "" else
                              concat [ flickrRequest
                                     , "&method=flickr.photos.getSizes"
                                     , "&photo_id=", id ]
      }
  in  get (requestSizes (getPhotoID (extract photoList)))


-- Take some size options and choose one, resulting in a URL.
sizesToPhoto sizeOptions =
  let getImg sizes =
          case reverse sizes of
          { _ : _ : _ : (JsonObject obj) : _ -> findString "source" obj
          ; (JsonObject obj) : _ -> findString "source" obj
          ; _ -> "waiting.gif" }
  in  getImg (findArray "size" (findObject "sizes" (extract sizeOptions)))


