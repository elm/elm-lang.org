
import Website.Docs

datatypes =
  [ ("data Response a = Waiting | Success a | Failure Int String", "", "The datatype for responses. Success contains only the returned message. Failures contain both an error code and an error message.")
  ]

singles =
  [ ("get", "String -> Signal (Response String)", "Performs an HTTP get request with the given address. Produces a signal that carries the response.")
  , ("post", "String -> Signal (Response String)", "Performs an HTTP post request with the given address. Produces a signal that carries the response.")
  ]

multis =
  [ ("gets", "Signal (Maybe String) -> Signal (Maybe (Response String))",
     "Produces a signal that carries 'Just' the response when given 'Just' a URL. Uses get requests.")
  , ("posts", "Signal (Maybe String) -> Signal (Maybe (Response String))",
     "Produces a signal that carries 'Just' the response when given 'Just' a URL. Uses post requests.")
  ]

categories =
  [ ("Responses", datatypes)
  , ("Single Requests", singles)
  , ("Multiple Requests", multis)
  ]

main = createDocs "HTTP" categories