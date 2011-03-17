module Compress(compressAndUpdate) where

import Diff
import Path
import System.Process
import System.Exit
import System.Directory(removeFile)
import List
import UpdateFile
import Data.List.Utils (endswith, replace)

compressInto src dest = do
    putStrLn $ "Compressing " ++ src
    result <- readProcessWithExitCode "lame" [src, dest] []
    case result of
        (ExitSuccess, _, _) -> return ()
    
compressFile _ = return ()

compressAndUpdate :: Path -> Path -> Diff -> IO ()
compressAndUpdate src dest diff | shouldConvert diff = compressAndCopy src dest $ pathOf diff
    where shouldConvert (Add path) | endswith ".wav" path = True
          shouldConvert (Update path) | endswith ".wav" path = True
          shouldConvert _ = False
compressAndUpdate src dest diff = updateFile src dest diff

compressAndCopy :: Path -> Path -> Path -> IO ()
compressAndCopy src dest path = do
    let mp3Version = replace ".wav" ".mp3" path
    compressInto (src `subPath` path) (src `subPath` mp3Version)
    updateFile src dest $ Update mp3Version
    removeFile (src `subPath` mp3Version) 