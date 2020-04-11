module Main where

import Clash.Prelude
import Circuit
import Sim
import Control.Monad

main :: IO ()
main = do
    sim <- driveIO_ (simulate @System logicBoard) ()
    replicateM_ 100000 $ sim $ \() -> return ()
