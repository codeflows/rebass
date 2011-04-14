{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Path where

import Data.List  
import ListUtil

type Path = String   

class Pathy a where
    pathOf :: a -> Path
    
instance Pathy Path where
    pathOf path = path
    
samePath :: Pathy a => Pathy b => a -> b -> Bool
samePath a b = pathOf a == pathOf b

parentOf :: Pathy a => Pathy b => a -> b -> Bool
parentOf parent child = (pathOf parent) ++ "/" `elem` inits (pathOf child)

parent :: Pathy a => a -> Path
parent path = reverse $ (drop n) $ reverse $ pathOf path
    where n = 1 + length (lastPathElement path)

subPath :: Pathy a => Pathy b => a -> b -> Path
subPath root sub = (pathOf root) ++ "/" ++ (pathOf sub)

lastPathElement :: Pathy a => a -> Path
lastPathElement path = head $ reverse $ split '/' (pathOf path)

isAbsolutePath :: Pathy a => a -> Bool
isAbsolutePath path = (pathOf path) `startsWith` "/"