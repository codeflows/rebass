module Diff where

import Status

data Diff = Added File | Removed File

diff :: [File] -> [File] -> [Diff]       
diff [] [] = []
diff (old:olds) [] = Removed old : diff olds []
diff [] (new:news) = Added new : diff [] news
diff (old:olds) (new:news) 
    | oldPath  < newPath = Removed old : diff olds (new:news)
    | oldPath  > newPath = Added new : diff (old:olds) news
    | otherwise          = compareFile old new ++ diff olds news
    where oldPath = pathOf old
          newPath = pathOf new


compareFile :: File -> File -> [Diff]
compareFile old new = []