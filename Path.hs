module Path where

type Path = String   

class Pathy a where
    pathOf :: a -> Path