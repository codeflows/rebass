import Status
import ReadFiles
import Diff

main = do 
        newStatus <- readStatus $ "."
        oldStatus <- loadStatus
        let diff = compareFile oldStatus newStatus
        putStrLn $ show diff
        saveStatus newStatus        