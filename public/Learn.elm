import WebSocket
import Mouse
import Json
import Dict
-- User land
 
unconfirmed : Signal String
unconfirmed = WebSocket.connect "ws://ws.blockchain.info/inv" (constant "{\"op\":\"unconfirmed_sub\"}")
 
clean : String -> Element
clean t = 
  (Json.fromString t)   |> 
  delve [ "x", "hash" ] |>
  mbind decodeStr       |>
  getOrElse ""          |>
  asText
 
main = lift clean unconfirmed
 
-- Library land
 
delve : [String] -> Maybe Json.Value -> Maybe Json.Value
delve xs mv = reducejson (map (\x -> getProp x) xs) mv
 
reducejson : [Json.Value -> Maybe Json.Value] -> Maybe Json.Value -> Maybe Json.Value
reducejson xs mv = foldl (\f b -> mbind f b) mv xs 
 
getProp : String -> Json.Value -> Maybe Json.Value
getProp n json = case json of
                  Json.Object d -> Just (Dict.getOrElse (Json.String "") n d)
                  _ -> Nothing
 
decodeStr : Json.Value -> Maybe String
decodeStr v = case v of
                Json.String s -> Just s
                _ -> Nothing
 
getStr : String -> Json.Value -> Maybe String
getStr n json = mbind decodeStr (getProp n json)
 
getOrElse : a -> Maybe a -> a
getOrElse b ma = case ma of
                 Just a -> a
                 Nothing -> b
                 
mbind : (a -> Maybe b) -> Maybe a -> Maybe b
mbind f ma = case ma of
                   Just a -> f a
                   _ -> Nothing
                   
mmap : (a -> b) -> Maybe a -> Maybe b
mmap f = mbind (\a -> Just (f a))
