module StringUtil where

split :: Eq a => a -> [a] -> [[a]]
split s xs = foldr collect ([[]]) xs
    where collect x (current : splitted)  | x == s    = [] : (current : splitted)
                                          | otherwise = (x : current) : splitted
   