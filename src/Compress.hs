module Compress(compressAndUpdate, compressInto) where

import Diff
import Path
import System.Process
import System.Exit
import System.Directory(removeFile)
import List
import UpdateFile 
import ListUtil (endsWith, replace)

compressInto :: Path -> Path -> IO ()
compressInto src dest = do
    putStrLn $ "Compressing " ++ src ++ " -> " ++ dest
    result <- readProcessWithExitCode "lame" [src, dest] []
    case result of
        (ExitSuccess, _, _) -> return ()
    
compressFile _ = return ()

compressAndUpdate :: Path -> Path -> Diff -> IO ()
compressAndUpdate src dest diff | shouldConvert diff = compressAndCopy src dest $ pathOf diff
    where shouldConvert (Add path) | path `endsWith` ".wav" = True
          shouldConvert (Update path) | path `endsWith` ".wav" = True
          shouldConvert _ = False
compressAndUpdate src dest diff = updateFile src dest diff

compressAndCopy :: Path -> Path -> Path -> IO ()
compressAndCopy src dest path = compressInto (subPath src path) (replace ".wav" ".mp3" (subPath dest path))                                              