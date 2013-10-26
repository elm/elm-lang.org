{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,
  TemplateHaskell,GeneralizedNewtypeDeriving,RecordWildCards,TypeFamilies #-}
module Gist (
  XKCD(..)
  ,Gist(..)
  ,GistDB(..)
  ,WordDB(..)
  ,initialGistDB
  ,initialWordDB
  ,UpdateGist(..)
  ,GetGist(..)
  ,GetWords(..)
  ,MemberWords(..)
  ,makeWordsUp
  ) where

import Happstack.Server
import Data.Typeable
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Control.Monad.State (get,put)
import Control.Monad.Reader (ask)
import Data.Hashable
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad.Trans(MonadIO(liftIO))
import System.Random (randomRIO)
import Control.Monad(replicateM)

initialGistDB :: GistDB
initialGistDB = GistDB IM.empty

initialWordDB :: IO WordDB
initialWordDB = do
  w <- readFile "resources/misc/wordlist.txt"
  return $ WordDB $ M.fromList $ zip (lines w) (repeat ())

type Word = String
newtype XKCD = XKCD {unXKCD::(Word,Word,Word,Word)}
  deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy,Hashable)
newtype Gist = Gist String
  deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy)
newtype GistDB = GistDB {getGistDB:: IM.IntMap Gist}
  deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy)
newtype WordDB = WordDB {getWordDB:: M.Map Word ()}
  deriving (Eq, Ord, Read, Show, Data, Typeable,SafeCopy)

updateGist :: XKCD -> Gist -> Update GistDB ()
updateGist xkcd gist = do
   db@GistDB{..} <- get
   put $ db {getGistDB=IM.insert (hash xkcd) gist getGistDB}

getGist :: Int -> Query GistDB Gist
getGist xkcd = do
   db@GistDB{..} <- ask
   return $ IM.findWithDefault (Gist "") xkcd getGistDB

memberWords :: [Word] -> Query WordDB Bool
memberWords w = do
  db@WordDB{..} <- ask
  return $ all (flip M.member getWordDB) w

getWords :: [Int] -> Query WordDB [Word]
getWords i = do
  db@WordDB{..} <- ask
  return $ map (fst . flip M.elemAt getWordDB) i

$(makeAcidic ''GistDB
  [ 'updateGist
  , 'getGist
  ])
$(makeAcidic ''WordDB
  [ 'memberWords
  , 'getWords
  ])

-- | Create four random words from the wordlist database.
makeWordsUp :: MonadIO m => AcidState (EventState GetWords) -> m XKCD
makeWordsUp acidW = do
  indices <- liftIO $ replicateM 4 $ randomRIO (0,53044)
  [a,b,c,d] <- query' acidW (GetWords indices)
  return $ XKCD (a,b,c,d)
