import Status
import ReadFiles

main = do 
        stuffz <- readStatus $ "."
        putStrLn $ show $ stuffz