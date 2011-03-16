module Compress(compressAndUpdate) where

import Diff
import Path
import System.Process
import List
import UpdateFile

compress :: Path -> [Diff] -> IO ()
compress path diffs = mapM_ (compressDiff path) diffs

compressDiff root (Add path) = compressFile (root `subPath` path)
compressDiff root (Update path) = compressFile (root `subPath` path)
compressDiff _ _ = return ()

compressFile path | ".wav" `elem` (tails path) = do
    putStrLn $ "Compressing " ++ path
    createProcess $ shell ("lame " ++ path)
    return ()
    
compressFile _ = return ()

compressAndUpdate :: Path -> Path -> Diff -> IO ()
compressAndUpdate src dest diff = do
    compressDiff src diff
    updateFile src dest diff
    
    