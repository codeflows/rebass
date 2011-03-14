module Status where
                                 
import System.IO
import System.Time(CalendarTime)
import System.Directory(createDirectoryIfMissing)

type Path = String    

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show, Read)
            
data FileStatus = FileStatus { timeStamp :: CalendarTime, size :: Integer }                
            deriving (Show, Read, Eq)

pathOf :: File -> String
pathOf (RegularFile path _) = path
pathOf (Directory   path _) = path

saveStatus :: File -> IO ()
saveStatus root = do 
	 createDirectoryIfMissing True rebassDir
	 writeFile statusFile $ show root

loadStatus :: IO File     
loadStatus = do
     contents <- readFile statusFile
     let state = read contents :: File
     return state

rebassDir = ".rebass"
statusFile = rebassDir ++ "/status"