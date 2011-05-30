module Rebass.Cache where

import Rebass.RebassProject
import Rebass.FileStatus
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Rebass.Path
import Control.Monad(liftM)

statusFileFor name = fmap (`subPath` name) rebassDir
	
createRebassDir = rebassDir >>= createDirectoryIfMissing True

rebassDir = return ".rebass"

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

projectStatusFile projectName = statusFileFor $ projectName ++ ".status"
