import Control.Concurrent
import Control.Monad (forever)
import Data.Char
import Data.IORef

capitalizer = forever $ do
  line <- getLine
  putStrLn (map toUpper line)


counter = do
  count <- newIORef 0
  forever $ do
    current <- readIORef count
    print current
    writeIORef count (current + 1)
    threadDelay 1000000


main = do
  forkIO capitalizer
  counter
