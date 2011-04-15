module ReaperCrap where

import ReaperStuff

main = do
    status <- projectStatus "examples/PatrolCar.RPP"
    putStrLn $ show status
--main = getArgs >>= dumpProject    