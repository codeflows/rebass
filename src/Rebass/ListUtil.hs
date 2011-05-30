module Rebass.ListUtil where

import Data.List

split :: Eq a => a -> [a] -> [[a]]
split s xs = foldr collect [[]] xs
    where collect x (current : splitted)  | x == s    = [] : (current : splitted)
                                          | otherwise = (x : current) : splitted

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith hayStack needle = any (== needle) $ tails hayStack   

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith hayStack needle = any (== needle) $ inits hayStack   

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new hayStack | hayStack `startsWith` old = new ++ replace old new (drop (length old) hayStack)
                         | otherwise                 = head hayStack : replace old new (tail hayStack)
