module Status where
                    
import Path                                 
import System.IO
import System.Time(CalendarTime)

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show, Read, Eq)
            
data FileStatus = FileStatus { timeStamp :: CalendarTime, size :: Integer }                
            deriving (Show, Read, Eq)
            
instance Pathy File where             
    pathOf (RegularFile path _) = path
    pathOf (Directory   path _) = path
    
emptyDir = Directory "" []    
