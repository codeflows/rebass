module Compress where

import Diff
import Path

compress :: Path -> [Diff] -> IO ()
compress path diffs = return ()