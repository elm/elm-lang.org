
module Main where

import Data.Char (isSymbol)
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List (intercalate,(\\))
import Control.Applicative
import Text.JSON
import RenameTypes as Rename
import System.FilePath
import System.Directory

main = do
  libs <- fmap parse (readFile "../resources/docs.json")
  structure <- readFile "structure.json"
  mapM writeDocs (parseStructure libs structure)

writeDocs (name, code) = do putStrLn name
                            createDirectoryIfMissing True dir
                            writeFile fileName code
  where
    fileName =  dir </> last fileParts <.> "elm"

    dir = ".." </> "public" </> "docs" </> joinPath (init fileParts)
    fileParts = split name

    split [] = []
    split xs = hd : split (dropWhile (=='.') tl)
        where (hd,tl) = span (/='.') xs

parseStructure libs s =
    map (toElm libs) . extract $ decodeStrict s

toElm libraries structure = (name, code)
  where
    code = concat [ "\nimport Website.Docs (createDocs)\n\n"
                  , "sections =", listify 1 sections, "\n\n"
                  , "description = [markdown|", desc, "|]\n\n"
                  , "main = createDocs \"", name, "\" description sections\n" ]
    name = extract (get "module" structure)
    desc = extract (get "description" structure)
    sections = map toSection (ss ++ rest)
        where gets obj = (,) <$> get "name" obj <*> valFromObj "values" obj
              ss = extract (mapM gets =<< valFromObj "sections" structure)
              leftovers = Map.keys facts \\ concatMap snd ss
              rest = if null leftovers then [] else []
                     --[("Other Useful Functions", leftovers)]

    find err = let msg = "Lookup Error: " ++ err ++ " was not found"
               in  Map.findWithDefault (error msg)

    facts = find ("module " ++ name) name libraries
    listify indent xs = spc ++ "[ " ++ intercalate (spc ++ ", ") xs ++ spc ++ "]"
        where spc = '\n' : replicate indent ' '
    toSection (name,values) = 
        "(\"" ++ name ++ "\"," ++ listify 4 (map toEntry values) ++ ")"
    isOp c = isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"
    toEntry value =
        "(\"" ++ value' ++ "\", \"" ++ tipe' ++ "\", [markdown|" ++ desc ++ "|])"
        where (tipe, desc) = find (name ++ "." ++ value) value facts
              tipe' = Rename.rename tipe
              value' = if all isOp value then "(" ++ value ++ ")" else value

extract :: Result a -> a
extract result = case result of { Error err -> error err; Ok libs -> libs }

parse docs = extract $ do
               obj <- decodeStrict docs
               modules <- valFromObj "modules" obj
               Map.fromList `fmap` mapM getValues modules

get :: String -> JSObject JSValue -> Result String
get = valFromObj

getValue obj = (,) <$> get "name" obj <*> pair
    where pair = (,) <$> get "type" obj <*> get "desc" obj

--getValues :: JSObject JSValue -> Result (String, Map.Map String String)
getValues obj = do
  name <- get "name" obj
  vs   <- valFromObj "values" obj
  vals <- mapM getValue vs
  return (name, Map.fromList vals)