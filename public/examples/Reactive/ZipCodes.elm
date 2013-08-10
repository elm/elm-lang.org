import Char
import Maybe
import Http
import Graphics.Input as Input


(field,rawInput) = Input.field "Zip Code"

-- Covert raw input into a usable URL.
toUrl s = if length s == 5 && all Char.isDigit s
             then Just ("http://zip.elevenbasetwo.com/v2/US/" ++ s)
             else Nothing

-- Transform the signal of raw input into usable data, indicating if the input
-- is valid and, if so, what it is.
realInput = lift toUrl rawInput

-- Send AJAX requests for any valid input!
responses = Http.sendGet (Maybe.maybe "" id <~ realInput)

-- Display a response.
display response = 
  case response of
    Http.Success address -> text . monospace <| toText address
    Http.Waiting -> image 16 16 "waiting.gif"
    Http.Failure _ _ -> asText response

-- Give the user a message depending on whether their input is valid and
-- the response from any AJAX requests.
message =
  let msg = plainText "Enter a valid zip code, such as 12345 or 90210."
      output inp rsp = Maybe.maybe msg (\_ -> display rsp) inp
  in lift2 output realInput responses

main = lift2 above field message
