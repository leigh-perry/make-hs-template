module Program
  ( run
  , isRelevantDir
  , isRelevantFile
  ) where

import Control.Monad

--import Debug.Trace
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (intercalate, isPrefixOf)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeExtension)
import System.Posix.Files (getFileStatus, isDirectory)

type Filename = String

type TopLevelPath = FilePath

type RelativePath = FilePath

type HsfilesPath = FilePath

type DirPredicate = TopLevelPath -> FilePath -> Bool

type FilePredicate = TopLevelPath -> FilePath -> Filename -> Bool

run :: IO ()
run = do
  args <- getArgs
  case args of
    [srcFilepath, summaryFile] -> templatise srcFilepath summaryFile
    _ -> putStrLn "Usage: make-hs-template <source dir> <target .hsfiles file>"

templatise :: TopLevelPath -> HsfilesPath -> IO ()
templatise srcFilepath summaryFile = do
  files <- relevantFiles srcFilepath srcFilepath isRelevantDir isRelevantFile
  writeTo srcFilepath summaryFile files
  traverse_ putStrLn files

isRelevantDir :: DirPredicate
isRelevantDir srcFilepath path = not $ any (`isPrefixOf` pathWithSlash) exclusionList
  where
    pathWithSlash = path <> "/"
    exclusionList = [".git", ".idea", ".stack-work", "out"] <&> (\s -> (srcFilepath </> s) <> "/")

isRelevantFile :: FilePredicate
isRelevantFile _ _ filename = filename `notElem` [".DS_Store", "stack.yaml.lock"] && takeExtension filename /= ".iml"

relevantFiles :: TopLevelPath -> FilePath -> DirPredicate -> FilePredicate -> IO [RelativePath]
relevantFiles srcFilepath dir includeDirPredicate includeFilePredicate = do
  ds <- listDirectory dir
  paths <-
    forM ds $ \filename -> do
      let filepath = dir </> filename
      s <- getFileStatus filepath
      if isDirectory s
        then if includeDirPredicate srcFilepath filepath
               then relevantFiles srcFilepath filepath includeDirPredicate includeFilePredicate
               else return []
        else if includeFilePredicate srcFilepath filepath filename
               then return [filepath]
               else return []
  return $ concat paths

writeTo :: TopLevelPath -> HsfilesPath -> [RelativePath] -> IO ()
writeTo srcFilepath summaryFile files = do
  contents <- forM files $ fileContents srcFilepath
  writeFile summaryFile $ intercalate "\n" contents

fileContents :: TopLevelPath -> FilePath -> IO String
fileContents srcFilepath filename =
  let prefixRemoved = dropWhile (== '/') $ drop (length srcFilepath) filename
   in readFile filename <&> (\s -> "{-# START_FILE " <> prefixRemoved <> " #-}" <> "\n" <> s)
