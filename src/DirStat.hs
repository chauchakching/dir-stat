module DirStat where

import qualified System.Directory              as D
import System.IO (FilePath)
import System.FilePath.Posix 
import Data.List
import Data.ByteUnits (ByteUnit(Bytes), ByteValue(..), getShortHand, getAppropriateUnits)
import Debug.Trace (trace)

listFiles :: String -> IO ()
listFiles targetPath = do
  directoryList <- D.listDirectory targetPath
  -- mapM_ putStrLn directoryList
  putStrLn $ prettyList directoryList

listCurrentDirectorySizes :: IO ()
listCurrentDirectorySizes = listDirectorySizes =<< D.getCurrentDirectory

listDirectorySizes :: String -> IO ()
listDirectorySizes path = do
  sizesInfo <- getDirectorySizes path
  putStrLn $ prettyList $ map (\(s,n) -> s ++ " ------- " ++ prettyByte n) sizesInfo

getDirectorySizes :: String -> IO [(FilePath, Integer)]
getDirectorySizes path = do
  isFile <- D.doesFileExist path
  isDirectory <- D.doesDirectoryExist path
  isSymbolicLink <- D.pathIsSymbolicLink path
  if isFile
    then do
      fileSize <- D.getFileSize path
      return [(path, fileSize)]
    else if isDirectory && not isSymbolicLink
      then do
        childrenPaths <- listDirectoryPaths path
        -- trace ("children paths: \n" ++ (prettyList childrenPaths)) (return [])
        zip childrenPaths <$> mapM getPathSize childrenPaths
      else return []

getPathSize :: String -> IO Integer
getPathSize path = do
  isFile <- D.doesFileExist path
  isDirectory <- D.doesDirectoryExist path
  isSymbolicLink <- D.pathIsSymbolicLink path
  if isFile
    then D.getFileSize path
    else if isDirectory && not isSymbolicLink
      then do
        childrenPaths <- listDirectoryPaths path
        sum <$> mapM getPathSize childrenPaths
      else return 0

listDirectoryPaths :: FilePath -> IO [FilePath]
listDirectoryPaths path = map (path </>) <$> D.listDirectory path

prettyList :: Show a => [a] -> String
prettyList xs = "[\n" ++ (unlines $ map (("  " ++) . show) xs) ++ "]"

prettyByte :: Integer -> String
prettyByte bytes = getShortHand $ getAppropriateUnits $ ByteValue (fromInteger bytes) Bytes