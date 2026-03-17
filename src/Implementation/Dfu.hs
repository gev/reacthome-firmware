{- HLINT ignore "Use newtype instead of data" -}
module Implementation.Dfu where

import Control.Monad.State
import Core.Context
import Core.Controller
import Ivory.Language
import Support.CMSIS.CoreCMFunc (disableIRQ, setMSP)
import Support.RunAppByAddr

data DFU = forall t. DFU
    { transport :: t
    }

dfu :: (Monad m, MonadState Context m) => m t -> m DFU
dfu transport' = do
    transport <- transport'
    addInit "jump_to_firmware" $ jumpToFirmware 0x8_002_000
    pure DFU{transport}

jumpToFirmware :: Uint32 -> Ivory eff ()
jumpToFirmware address = do
    disableIRQ
    setMSP address
    startFirmware address

startFirmware :: Uint32 -> Ivory eff ()
startFirmware address = do
    runAppByAddr $ address + 4

instance Controller DFU where
    handle _ _ _ = pure ()