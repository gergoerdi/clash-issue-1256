module Circuit where

import Clash.Prelude
import Data.Word
import Control.Monad.State

type PC = Unsigned 12

logicBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom ()
    -> Signal dom ()
logicBoard x = dummy <$> cpuOut
  where
    cpuOut = cpu cpuIn
    romRead = withStart 0 $ blockRam1 NoClearOnReset (SNat @10000) 0 (progAddr <$> cpuOut) (pure Nothing)
    cpuIn = CPUIn <$> romRead

withStart x = mux (register True $ pure False) (pure x)

data CPUIn = CPUIn
    { instr :: Word8
    }
    deriving (Generic, NFDataX)

data CPUOut = CPUOut
    { progAddr :: PC
    , dummy :: ()
    }
    deriving (Generic, NFDataX)

defaultOutput :: PC -> CPUOut
defaultOutput pc = CPUOut
    { progAddr = pc
    , dummy = ()
    }

cpu :: (HiddenClockResetEnable dom) => Signal dom CPUIn -> Signal dom CPUOut
cpu = mealy step 0
  where
    step s CPUIn{} = let (y, s') = (defaultOutput s, s + 1) in (s', y)
