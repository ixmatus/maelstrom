import           Control.Concurrent      (ThreadId, forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad           (forM, forever)

threadPool :: Int -> (a -> b) -> IO ([ThreadId], Chan a, Chan b)
threadPool nr f = do
    input  <- newChan
    output <- newChan

    thrds  <- forM [1..nr] $
        \_ -> forkIO (forever $ do
                        i <- readChan input
                        o <- return $! f i
                        writeChan output o)
    return (thrds, input, output)


appendToTMV tv v = do
    tmvar <- tv
    val   <- takeTMVar tmvar

    putTMVar tmvar (v:val)
