module Sim where

import Clash.Prelude
import Control.Concurrent

driveIO_ :: ([i] -> [o]) -> i -> IO ((o -> IO i) -> IO ())
driveIO_ circuit input0 = do
    inChan <- newChan
    writeChan inChan input0

    ins <- getChanContents inChan
    outs <- newMVar $ circuit ins

    return $ \world -> do
        out <- modifyMVar outs $ \(out:outs) -> return (outs, out)
        input <- world out
        writeChan inChan input
