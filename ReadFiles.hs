module ReadFiles (readStatus, handleFileOrDirectory) where
                             
import Status
import Path
import System.Time(ClockTime, toCalendarTime)
import System.Directory(getDirectoryContents, doesDirectoryExist, getModificationTime)
import System.Posix.Files(getFileStatus, fileSize)                                                              
import Data.List(sort)
                
ignored = [".", "..", ".git", ".gitignore", ".rebass"]

readStatus :: Path -> IO File
readStatus path = handleFileOrDirectory path readFileStatus readDirectoryStatus
    where
        readFileStatus path = do
            timestamp <- getModificationTime path >>= toCalendarTime
            posixFileStatus <- getFileStatus path
            return $ RegularFile path $ FileStatus timestamp (toInteger $ fileSize posixFileStatus)

        readDirectoryStatus path = do      
            contents <- (getDirectoryContents path >>= (readDirectoryContents path))
            return $ Directory path contents                    

        readDirectoryContents parent paths = sequence $ map readStatus $ map fullPath $ filter realFile $ sort $ paths
            where
                  realFile path = not $ path `elem` ignored      
                  fullPath path = parent ++ "/" ++ path
                  
handleFileOrDirectory path fileHandler directoryHandler = 
    (doesDirectoryExist path) >>= (\dir -> if dir then (directoryHandler path) else (fileHandler path))                  