module Rebass.Reaper.Samples(Sample(..), samples, sampleFilePath) where

import Rebass.Path
import Rebass.Reaper.ReaperProject(Project, Node(Command, Container), Parameter(..))

data Sample = Sample { fileName :: String }
instance Pathy Sample where
    pathOf = fileName

samples :: Project -> [Sample]
samples (Command "FILE" [String fileName]) = [Sample fileName]
samples (Container name parameters children) = concatMap samples children
samples _ = []

sampleFilePath projectPath sample | isAbsolutePath sample = fileName sample
                                  | otherwise             = (parent projectPath) `subPath` sample
