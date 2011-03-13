module ReadFiles (readStatus) where
                             
import Status
import System.Time(ClockTime, toCalendarTime)
import System.Directory(getDirectoryContents, doesDirectoryExist, getModificationTime)
import System.Posix.Files(getFileStatus, fileSize)
                
ignored = [".", "..", ".git", ".rebass"]

readStatus :: Path -> IO File
readStatus path = (doesDirectoryExist path) >>= (\dir -> if dir then (readDirectoryStatus path) else (readFileStatus path))
    where
        readFileStatus path = do
            timestamp <- getModificationTime path >>= toCalendarTime
            posixFileStatus <- getFileStatus path
            return $ RegularFile path $ FileStatus timestamp (toInteger $ fileSize posixFileStatus)

        readDirectoryStatus path = do      
            contents <- (getDirectoryContents path >>= (readDirectoryContents path))
            return $ Directory path contents                    

        readDirectoryContents parent paths = sequence $ map readStatus $ map fullPath $ filter realFile $ paths
            where
                  realFile path = not $ path `elem` ignored      
                  fullPath path = parent ++ "/" ++ path                               