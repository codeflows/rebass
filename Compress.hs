module Compress where

import Diff
import Path
import System.Process
import List

compress :: Path -> [Diff] -> IO ()
compress path diffs = mapM_ (compressDiff path) diffs

compressDiff root (Add path) = compressFile root path
compressDiff root (Update path) = compressFile root path
compressDiff _ _ = return ()

compressFile root relative | ".wav" `elem` (tails relative) = do
    putStrLn $ "Compressing " ++ relative
    createProcess $ shell ("lame " ++ root ++ "/" ++ relative)
    return ()
    
compressFile _ _ = return ()    
    
    