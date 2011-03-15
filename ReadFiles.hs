module ReadFiles (readStatus, handleFileOrDirectory) where
                             
import Status
import Path
import System.Time(ClockTime, toCalendarTime)
import System.Directory(getDirectoryContents, doesDirectoryExist, getModificationTime)
import System.Posix.Files(getFileStatus, fileSize)                                                              
import Data.List(sort)
                
ignored = [".", "..", ".git", ".gitignore", ".rebass"]

readStatus :: Path -> IO File
readStatus root = read root
    where
        read path = handleFileOrDirectory path readFileStatus readDirectoryStatus

        readFileStatus path = do
            timestamp <- getModificationTime path >>= toCalendarTime
            posixFileStatus <- getFileStatus path
            return $ RegularFile (relativePath path) $ FileStatus timestamp (toInteger $ fileSize posixFileStatus)

        readDirectoryStatus path = do      
            contents <- (getDirectoryContents path >>= (readDirectoryContents path))
            return $ Directory (relativePath path) contents                    

        readDirectoryContents parent paths = sequence $ map read $ map fullPath $ filter realFile $ sort $ paths
            where
                  realFile path = not $ path `elem` ignored      
                  fullPath path = parent ++ "/" ++ path
        
        relativePath path = drop ((length root)+1) path
                  
handleFileOrDirectory path fileHandler directoryHandler = 
    (doesDirectoryExist path) >>= (\dir -> if dir then (directoryHandler path) else (fileHandler path))                  