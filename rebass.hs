import System.Time(ClockTime)
import System.Directory(getDirectoryContents, doesDirectoryExist)

type Path = String

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show)
            
data FileStatus = FileStatus { timeStamp :: ClockTime, size :: Integer }                
            deriving (Show)

readDirectoryStatus :: Path -> IO File
readDirectoryStatus path = do      
    contents <- (getDirectoryContents path >>= (readContents path))
    return $ Directory path contents                    

readContents :: Path -> [Path] -> IO [File]
readContents parent paths = sequence $ map readStatus $ map fullPath $ filter realFile $ paths
    where
          realFile path = not $ path `elem` [".", ".."]      
          fullPath path = parent ++ "/" ++ path
          
readStatus :: Path -> IO File
readStatus path = do
    isDir <- doesDirectoryExist path
    if isDir 
        then (readDirectoryStatus path) 
        else (return $ Directory path [])
    
main = do 
        stuffz <- readDirectoryStatus $ "."
        putStrLn $ show $ stuffz