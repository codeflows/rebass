module Path where

import Data.List 

type Path = String   

class Pathy a where
    pathOf :: a -> Path
    
samePath :: Pathy a => Pathy b => a -> b -> Bool
samePath a b = pathOf a == pathOf b

parentOf :: Pathy a => Pathy b => a -> b -> Bool
parentOf parent child = (pathOf parent) ++ "/" `elem` inits (pathOf child)

subPath :: Path -> Path -> Path
subPath root sub = root ++ "/" ++ sub