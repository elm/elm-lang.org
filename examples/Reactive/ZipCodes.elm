
component (fld, txt) =
  lift2 above
    (lift extract . HTTP.gets . lift toUrl $ txt)
    (constant . size 400 50 . box 5 $ plainText "Enter a US Zip Code:" `beside` fld)

main = component (Input.textField "")


--------  Convert user-input to valid JSON request  --------

sanitize s = if List.length s == 5 && onlyDigits s then Just s else Nothing
onlyDigits = let digits = ['0','1','2','3','4','5','6','7','8','9'] in
             List.forall (\c -> List.exists ((==)c) digits)

maybe f v = case v of { Just x -> Just (f x); Nothing -> Nothing }
url = "http://www.geonames.org/postalCodeLookupJSON?country=US&postalcode="
toUrl s = maybe ((++)url) (sanitize s)


--------  Extract city from JSON response  --------

-- This should get easier once JSON is properly supported in Elm.

takeWhile pred lst =
  case lst of { x:xs -> if pred x then x : takeWhile pred xs else [] ; [] -> [] }

getZip s =
  case s of
  { a:b:c:d:e:rest ->
      if a == 'm' && b == 'e' && c == '"'
         then takeWhile ((/=)'"') rest
         else getZip (b:c:d:e:rest)
  ; c:cs -> getZip cs
  ; [] -> "Not Found!" }

extractHelp response =
  case response of
  { Waiting -> image "/loading.gif"
  ; Success s -> plainText $ getZip s
  ; _ -> asText response }

extract responseOpt =
  case responseOpt of
  { Just s -> extractHelp s
  ; Nothing -> plainText "Invalid Zip Code: must be exactly 5 digits long. Maybe try '12345'." }