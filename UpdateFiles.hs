module UpdateFiles where

import Path
import Diff
import System.Directory
import ReadFiles(handleFileOrDirectory)

updateFiles :: Path -> Path -> [Diff] -> IO ()
updateFiles src _ [] =
        putStrLn $ "No changes in " ++ src
updateFiles src dest diffs =
        (putStrLn $ "Updating " ++ src ++ " --> " ++ dest)
        >> (mapM_ logAndUpdate diffs)
        >> (putStrLn $ "Done")
  where
        logAndUpdate diff = (putStrLn $ show diff) >> update diff
        update (Add file) = copyFile (srcPath file) (destPath file)
        update (MkDir dir) = createDirectoryIfMissing True (destPath dir)
        update (Rm path) = handleFileOrDirectory (destPath path) removeFile removeDirectoryRecursive
        update (Update file) = copyFile (srcPath file) (destPath file)
        destPath path = dest ++ "/" ++ path
        srcPath path = src ++ "/" ++ path
        