
import HTTP
import JSON

(zipPicker, zipCode) = Input.stringDropDown [ "10001", "90210", "12345" ]
    
detail =
  let toRequest s = get $ "http://zip.elevenbasetwo.com/v2/US/" ++ s in
  lift extract . send $ lift toRequest zipCode
           
extract response =
  case response of
    Success json -> plainText . findString "city" $ fromString json
    Waiting -> image 16 16 "waiting.gif"
    Failure _ _ -> asText response

display info =
  flow right [ zipPicker, plainText " is the zip code for ", info ]
  
main = lift display detail