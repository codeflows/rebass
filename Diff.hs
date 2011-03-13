module Diff where

import Status

data Diff = Added File | Removed File | Replaced File | Updated File

diff :: [File] -> [File] -> [Diff]       
diff [] [] = []
diff (old:olds) [] = Removed old : diff olds []
diff [] (new:news) = Added new : diff [] news
diff (old:olds) (new:news) 
    | oldPath  < newPath = Removed old : diff olds (new:news)
    | oldPath  > newPath = findAdded new ++ diff (old:olds) news
    | otherwise          = compareFile old new ++ diff olds news
    where oldPath = pathOf old
          newPath = pathOf new

findAdded :: File -> [Diff]
findAdded new@(RegularFile _ _) = [Added new]
findAdded new@(Directory _ contents) = Added new : (concat $ map findAdded contents)

compareFile :: File -> File -> [Diff]
compareFile (RegularFile _ _) new@(Directory _ _) = [Replaced new]
compareFile (Directory _ _) new@(RegularFile _ _) = [Replaced new]   
compareFile (RegularFile _ oldStatus) new@(RegularFile _ newStatus) 
    | (oldStatus == newStatus) = []
    | otherwise                = [Updated new]
compareFile (Directory _ oldContents) new@(Directory _ newContents) 
    = diff oldContents newContents
