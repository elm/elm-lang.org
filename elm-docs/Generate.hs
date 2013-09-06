{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Applicative
import Control.Monad
import Data.Aeson
import System.FilePath
import System.Directory

main = do
  src <- BS.readFile "../resources/docs.json"
  return (eitherDecode src :: Either String Document)

writeDocs (name, code) =
  do putStrLn name
     createDirectoryIfMissing True dir
     writeFile fileName code
  where
    fileName =  dir </> last fileParts <.> "elm"

    dir = ".." </> "public" </> "docs" </> joinPath (init fileParts)
    fileParts = split name

    split [] = []
    split xs = hd : split (dropWhile (=='.') tl)
        where (hd,tl) = span (/='.') xs

data Document = Doc
    { moduleName :: String
    , structure :: String
    , entries :: [Entry]
    } deriving (Show)

data Entry = Entry
    { name :: String
    , comment :: String
    , raw :: String
    , assocPrec :: Maybe (String,Int)
    } deriving (Show)

instance FromJSON Document where
    parseJSON (Object v) =
        Doc <$> v .: "name"
            <*> v .: "document"
            <*> (concat <$> sequence [ v .: "aliases", v .: "datatypes", v .: "values" ])

    parseJSON _ = fail "Conversion to Document was expecting an object"

instance FromJSON Entry where
    parseJSON (Object v) =
        Entry <$> v .: "name"
              <*> v .: "comment"
              <*> v .: "raw"
              <*> (liftM2 (,) <$> v .:? "associativity"
                              <*> v .:? "precedence")

    parseJSON _ = fail "Conversion to Entry was expecting an object"
