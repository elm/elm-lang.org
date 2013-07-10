module DirTree where

import Data.List (isPrefixOf)
import System.Directory (doesFileExist, getCurrentDirectory, getDirectoryContents, setCurrentDirectory)
import System.FilePath ((</>))

data DirTree = File String
             | Dir String [DirTree]
             deriving Show

buildDirTree :: FilePath -> IO DirTree
buildDirTree p = do
  bool <- doesFileExist p
  if bool
    then return $ File p
    else do
    cur <- getCurrentDirectory
    setCurrentDirectory p
    tree <- do
      dirs <- getDirectoryContents "."
      let notDots = filter (not . ("." `isPrefixOf`)) dirs
      dirTrees <- mapM buildDirTree notDots
      return $ Dir p dirTrees
    setCurrentDirectory cur
    return tree
  
directories :: DirTree -> [FilePath]
directories (File _)     = []
directories (Dir pre ds) = pre : (map (pre </>) (ds >>= directories))

files :: DirTree -> [FilePath]
files (File f)     = return f
files (Dir pre ds) = map (pre </>) (ds >>= files)
