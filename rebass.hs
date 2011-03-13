import System.Time(ClockTime)
import System.Directory(getDirectoryContents)

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
readContents parent paths = return $ map simpleFile $ map fullPath $ filter realFile $ paths
    where simpleFile path = Directory path []                   
          realFile path = not $ path `elem` [".", ".."]      
          fullPath path = parent ++ "/" ++ path
    
main = do 
        stuffz <- readDirectoryStatus $ "."
        putStrLn $ show $ stuffz