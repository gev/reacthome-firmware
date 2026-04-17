module Implementation.Dfu where

import Control.Monad.Reader
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Transport
import Data.Word
import Feature.GetInfo
import Ivory.Language
import Ivory.Stdlib
import Support.CMSIS.CoreCMFunc
import Support.ReadAddr
import Support.RunAppByAddr
import Core.Task

data DFU = forall t. DFU
    { info :: GetInfo
    , transport :: t
    }

dfu ::
    ( Monad m
    , MonadState Context m
    , LazyTransport t
    , MonadReader (D.Domain p i) m
    ) =>
    Int -> (Word8, Word8) -> m t -> m DFU
dfu address version transport' = do
    transport <- transport'
    addInit "jump_to_firmware" $ jumpToFirmware $ fromIntegral address
    info <- mkGetDfuInfo version transport
    pure DFU{info, transport}

jumpToFirmware :: Uint32 -> Ivory eff ()
jumpToFirmware address = do
    disableIRQ
    setMSP =<< readAddr32u address
    runAppByAddr $ address + 4

instance Controller DFU where
    handle DFU{..} buff _ = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionGetInfo ==> onGetInfo info
            ]
