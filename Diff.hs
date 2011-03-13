module Diff where

import Status

data Diff = Add Path | MkDir Path | Rm Path | Update Path
    deriving (Show, Read)

diff :: [File] -> [File] -> [Diff]       
diff [] [] = []
diff (old:olds) [] = Rm (pathOf old) : diff olds []
diff [] (new:news) = findAdded new ++ diff [] news
diff (old:olds) (new:news) 
    | oldPath  < newPath = Rm oldPath : diff olds (new:news)
    | oldPath  > newPath = findAdded new ++ diff (old:olds) news
    | otherwise          = compareFile old new ++ diff olds news
    where oldPath = pathOf old
          newPath = pathOf new

findAdded :: File -> [Diff]
findAdded (RegularFile path _) = [Add path]
findAdded (Directory path contents) = MkDir path : (concat $ map findAdded contents)

compareFile :: File -> File -> [Diff]
compareFile (RegularFile path _) new@(Directory _ _) = [Rm path] ++ findAdded new
compareFile (Directory path _) new@(RegularFile _ _) = [Rm path] ++ findAdded new   
compareFile (RegularFile path oldStatus) (RegularFile _ newStatus) 
    | (oldStatus == newStatus) = []
    | otherwise                = [Update path]
compareFile (Directory _ oldContents) new@(Directory _ newContents) 
    = diff oldContents newContents
