
import HTTP
import List (intercalate)
import JSON

----  Core Logic  ----

-- asynchronously get a photo with a given tag
getPhotos tags =
  let photoList  = send (lift requestTag tags) in
  let photoSizes = send (lift requestOneFrom photoList) in
      lift sizesToPhoto photoSizes

-- create a text input box
(tagInput, tags) = Input.textField "Request Tag"

-- put our text input and images together
scene (w,h) img =
    flow down [ container w 60 middle tagInput
              , container w (h - 100) middle img ]

-- show everything on screen
main = lift2 scene Window.dimensions (images (getPhotos (dropRepeats tags)))



----  Helper Functions  ----


-- the standard parts of every Flickr API request
flickrRequest =
  "http://api.flickr.com/services/rest/?format=json" ++
  "&nojsoncallback=1&api_key=66c61b93c4723c7c3a3c519728eac252"


-- extract a JSON object from a HTTP response
extract response =
  case response of
  { Success str -> JSON.fromString str
  ; _ -> empty }


-- turn a tag into a request
requestTag tag =
  if tag == "" then get "" else
  get (concat [ flickrRequest
              , "&method=flickr.photos.search"
              , "&sort=random"
              , "&tags=", tag
              , "&per_page=10" ])


-- take a list of photos and choose one
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


-- take some size options and choose one
sizesToPhoto sizeOptions =
  let getImg sizes =
          case reverse sizes of
          { _ : _ : _ : (JsonObject obj) : _ -> findString "source" obj
          ; (JsonObject obj) : _ -> findString "source" obj
          ; _ -> "waiting.gif" }
  in  getImg (findArray "size" (findObject "sizes" (extract sizeOptions)))


