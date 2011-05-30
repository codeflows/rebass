module Rebass.UpdateFiles where

import Rebass.Path
import Rebass.Diff
import Rebass.UpdateFile
import Rebass.Compress

updateFiles :: Path -> Path -> [Diff] -> IO ()
updateFiles src _ [] =
        putStrLn $ "No changes in " ++ src
updateFiles src dest diffs =
        (putStrLn $ "Updating " ++ src ++ " --> " ++ dest)
        >> (mapM_ (logAndUpdate src dest) diffs)
        >> (putStrLn $ "Done")

logAndUpdate src dest diff = do
    print diff
    (compressAndUpdate src dest) diff
        
    
        
