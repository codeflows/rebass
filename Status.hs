module Status where
                                 
import System.IO
import System.Time(CalendarTime)

type Path = String    

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show, Read)
            
data FileStatus = FileStatus { timeStamp :: CalendarTime, size :: Integer }                
            deriving (Show, Read, Eq)

pathOf :: File -> String
pathOf (RegularFile path _) = path
pathOf (Directory   path _) = path