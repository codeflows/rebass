import System.Time(ClockTime)
import System.Directory(getDirectoryContents, doesDirectoryExist, getModificationTime)
import System.Posix.Files(getFileStatus, fileSize)

type Path = String    

ignored = [".", "..", ".git"]

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show)
            
data FileStatus = FileStatus { timeStamp :: ClockTime, size :: Integer }                
            deriving (Show)

readDirectoryStatus :: Path -> IO File
readDirectoryStatus path = do      
    contents <- (getDirectoryContents path >>= (readDirectoryContents path))
    return $ Directory path contents                    

readDirectoryContents :: Path -> [Path] -> IO [File]
readDirectoryContents parent paths = sequence $ map readStatus $ map fullPath $ filter realFile $ paths
    where
          realFile path = not $ path `elem` ignored      
          fullPath path = parent ++ "/" ++ path
          
readFileStatus :: Path -> IO File
readFileStatus path = do
    timestamp <- getModificationTime path
    posixFileStatus <- getFileStatus path
    return $ RegularFile path $ FileStatus timestamp (toInteger $ fileSize posixFileStatus)
          
readStatus :: Path -> IO File
readStatus path = (doesDirectoryExist path) >>= (\dir -> if dir then (readDirectoryStatus path) else (readFileStatus path))
    
main = do 
        stuffz <- readDirectoryStatus $ "."
        putStrLn $ show $ stuffz