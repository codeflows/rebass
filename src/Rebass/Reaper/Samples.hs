module Rebass.Reaper.Samples(Sample(..), samples, sampleFilePath) where

import Rebass.Diff(Diff, diff)
import Rebass.Path
import System.Directory   
import Rebass.Compress(compressInto)  
import Rebass.ListUtil(replace)
import Rebass.Reaper.ReaperProject(Project, Node(Command, Container), Parameter(..))
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile)
import System.Environment(getArgs)
import Rebass.Status(File(RegularFile))
import Control.Monad
import Rebass.ReadFiles(readFileStatus)

data Sample = Sample { fileName :: String }
instance Pathy Sample where
    pathOf = fileName

samples :: Project -> [Sample]
samples (Command "FILE" [String fileName]) = [Sample fileName]
samples (Container name parameters children) = concatMap samples children
samples _ = []

ifNotExists :: Path -> IO () -> IO ()
ifNotExists file action = doesFileExist file >>= ifElse (return ()) action
    
ifElse :: a -> a -> Bool -> a
ifElse a b True = a
ifElse a b False = b        
    
sampleFilePath projectPath sample | isAbsolutePath sample = fileName sample
                                  | otherwise             = (parent projectPath) `subPath` sample
    
