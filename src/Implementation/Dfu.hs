{- HLINT ignore "Use newtype instead of data" -}
module Implementation.Dfu where

import Control.Monad.State
import Core.Context
import Core.Controller
import Ivory.Language
import Support.CMSIS.CoreCMFunc
import Support.ReadAddr
import Support.RunAppByAddr

data DFU = forall t. DFU
    { transport :: t
    }

dfu :: (Monad m, MonadState Context m) => Uint32 -> m t -> m DFU
dfu address transport' = do
    transport <- transport'
    addInit "jump_to_firmware" $ jumpToFirmware address
    pure DFU{transport}

jumpToFirmware :: Uint32 -> Ivory eff ()
jumpToFirmware address = do
    -- disableIRQ
    setMSP =<< readAddr32u address
    runAppByAddr $ address + 4

instance Controller DFU where
    handle _ _ _ = pure ()