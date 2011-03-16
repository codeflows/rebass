module Compress where

import Diff
import Path

compress :: Path -> [Diff] -> IO ()
compress path diffs = mapM_ (compressDiff path) diffs

compressDiff root (Add path) = compressFile root path
compressDiff root (Update path) = compressFile root path
compressDiff _ _ = return ()

compressFile root relative = return ()