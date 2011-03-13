module Status where

import System.Time(ClockTime)

type Path = String    

data File = RegularFile { path :: Path, status :: FileStatus } 
            | Directory { path :: Path, contents :: [File]}
            deriving (Show)
            
data FileStatus = FileStatus { timeStamp :: ClockTime, size :: Integer }                
            deriving (Show)
