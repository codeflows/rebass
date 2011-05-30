module Rebass.Cache(readCachedStatus, writeStatus) where

import Rebass.RebassProject
import Rebass.FileStatus
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Rebass.Path
import Control.Monad(liftM)

readCachedStatus :: String -> IO RebassProjectStatus
readCachedStatus projectName = do
    statusFile <- projectStatusFile projectName
    readFile statusFile >>= (return . read)

writeStatus :: RebassProject -> RebassProjectStatus -> IO ()
writeStatus project status = do
    statusFile <- projectStatusFile $ projectName project 
    createRebassDir
    putStrLn $ "Using status file " ++ statusFile
    writeFile statusFile $ show status 
  where createRebassDir = rebassDir >>= createDirectoryIfMissing True

projectStatusFile projectName = statusFileFor $ projectName ++ ".status"
  where statusFileFor name = fmap (`subPath` name) rebassDir

rebassDir = return ".rebass"
