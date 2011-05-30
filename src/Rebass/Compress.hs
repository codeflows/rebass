module Rebass.Compress(compressInto) where

import Rebass.Path
import System.Process
import System.Exit

compressInto :: Path -> Path -> IO ()
compressInto src dest = do
    putStrLn $ "Compressing " ++ src ++ " -> " ++ dest
    result <- readProcessWithExitCode "lame" [src, dest] []
    case result of
        (ExitSuccess, _, _) -> return ()

