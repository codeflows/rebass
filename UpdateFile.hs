module UpdateFile where

import Path
import Diff
import System.Directory
import ReadFiles(handleFileOrDirectory)

updateFile src dest (Add file) = copyFile (src `subPath` file) (dest `subPath` file)
updateFile src dest (MkDir dir) = createDirectoryIfMissing True (dest `subPath` dir)
updateFile src dest (Rm path) = handleFileOrDirectory (dest `subPath` path) removeFile removeDirectoryRecursive
updateFile src dest (Update file) = copyFile (src `subPath` file) (dest `subPath` file)    