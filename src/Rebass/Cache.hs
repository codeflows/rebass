module Rebass.Cache where

import Rebass.Status
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Rebass.Path
import Control.Monad(liftM)

remoteLocationFor name = do
    home <- getHomeDirectory
    return $ home ++ "/Dropbox/Rebass/" ++ name
    
statusFileFor name = fmap (`subPath` name) rebassDir
	
createRebassDir = rebassDir >>= createDirectoryIfMissing True

rebassDir = return ".rebass"
