module Rebass.Diff (Diff (Add, MkDir, Rm, Update), diff, getConflicts) where

import Rebass.Status
import Rebass.Path

data Diff = Add { path :: Path } | MkDir { path :: Path } | Rm { path :: Path } | Update { path :: Path }
    deriving (Show, Read)
    
instance Pathy Diff where
    pathOf = Rebass.Diff.path

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
findAdded (Directory path contents) = MkDir path : (concatMap findAdded contents)

compareFile :: File -> File -> [Diff]
compareFile (RegularFile path _) new@(Directory _ _) = [Rm path] ++ findAdded new
compareFile (Directory path _) new@(RegularFile _ _) = [Rm path] ++ findAdded new   
compareFile (RegularFile path oldStatus) (RegularFile _ newStatus) 
    | (oldStatus == newStatus) = []
    | otherwise                = [Update path]
compareFile (Directory _ oldContents) new@(Directory _ newContents) 
    = diff oldContents newContents
    
getConflicts :: ([Diff], [Diff]) -> [(Diff, Diff)]
getConflicts (local, remote) = filter isConflict [(a, b) | a <- local, b <- remote]
    where isConflict ((Rm _), (Rm _)) = False
          isConflict (a, b) 
                            | b `parentOf` a  = True
                            | a `parentOf` b  = True
                            | a `samePath` b  = True
                            | otherwise       = False                            