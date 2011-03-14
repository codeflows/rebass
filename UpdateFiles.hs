module UpdateFiles where

import Status
import Diff

updateFiles :: Path -> Path -> [Diff] -> IO ()
updateFiles from to diffs = do
        putStrLn $ "Updating " ++ from ++ " --> " ++ to
        mapM_ update diffs
  where
        update diff = putStrLn $ show diff