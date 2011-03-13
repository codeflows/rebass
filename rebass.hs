import Status
import ReadFiles
import Diff

main = do 
        newStatus <- readStatus $ "."
        oldStatus <- loadStatus
        -- saveStatus newStatus
        let diff = compareFile oldStatus newStatus
        putStrLn $ show diff