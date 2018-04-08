import Control.Monad.ST
import Data.STRef
import Control.Monad


sumInts :: [Int] -> Int
sumInts xs = runST $ do
    n <- newSTRef 0
    forM_ xs $ \x -> do
        modifySTRef n (+x)
    readSTRef n

