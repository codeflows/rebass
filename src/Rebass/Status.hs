module Rebass.Status where
                    
import Rebass.Path                                 
import System.IO
import System.Time(CalendarTime)
import System.Time(ClockTime, toCalendarTime)
import System.Directory(getDirectoryContents, doesDirectoryExist, getModificationTime)
import System.Posix.Files(getFileStatus, fileSize)                                                              
import Data.List(sort)
import Control.Monad(liftM2)
 
data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show, Read, Eq)
            
data FileStatus = FileStatus { timeStamp :: CalendarTime, size :: Integer }                
            deriving (Show, Read, Eq)
            
instance Pathy File where             
    pathOf (RegularFile path _) = path
    pathOf (Directory   path _) = path
    
emptyDir = Directory "" []    

ignored = [".", "..", ".git", ".gitignore", ".rebass"]

readFileStatus :: Path -> IO File
readFileStatus root = read root
    where
        read path = handleFileOrDirectory path readFile readDirectory

        readFile path = do
            timestamp <- getModificationTime path >>= toCalendarTime
            posixFileStatus <- getFileStatus path
            return $ RegularFile (relativePath path) $ FileStatus timestamp (toInteger $ fileSize posixFileStatus)

        readDirectory path = do      
            contents <- (getDirectoryContents path >>= (readDirectoryContents path))
            return $ Directory (relativePath path) contents                    

        readDirectoryContents parent paths = sequence $ map read $ map fullPath $ filter realFile $ sort $ paths
            where
                  realFile path = path `notElem` ignored      
                  fullPath path = parent ++ "/" ++ path
        
        relativePath path = drop ((length root)+1) path
                  
handleFileOrDirectory path fileHandler directoryHandler = 
    (doesDirectoryExist path) >>= (\dir -> if dir then (directoryHandler path) else (fileHandler path))
