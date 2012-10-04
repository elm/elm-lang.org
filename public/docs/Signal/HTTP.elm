
import Website.Docs

responses =
  [ ("data Response a = Waiting | Success a | Failure Int String", "", "The datatype for responses. Success contains only the returned message. Failures contain both an error code and an error message.")
  ]

requests =
  [ ("get"
    , "String -> Request String"
    , "Create a GET request to the given url."
    )
  , ("post"
    , "String -> String -> Request String"
    , "Create a POST request to the given url, carrying the given data."
    )
  , ("request"
    , "String -> String -> String -> [(String,String)] -> Request String"
    , "Create a customized request. Arguments are request type (get, post, put, delete, etc.), target url, data, and a list of additional headers."
    )
  ]

send =
  [ ("send"
    , "Signal (Request a) -> Signal (Response String)"
    , "Performs an HTTP request with the given requests. Produces a signal that carries the responses." )
  , ("sendGet"
    , "Signal String -> Signal (Response String)"
    , "Performs an HTTP GET request with the given urls. Produces a signal that carries the responses." )
  ]

categories =
  [ ("Sending Requests", send)
  , ("Responses", responses)
  , ("Creating Requests", requests)
  ]

main = createDocs "Signal.HTTP" categories