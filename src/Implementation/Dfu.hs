module Implementation.Dfu where

import Control.Monad.Reader
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Task (delay)
import Core.Transport
import Data.Word
import Feature.GetInfo
import Ivory.Language
import Ivory.Stdlib
import Support.CMSIS.CoreCMFunc
import Support.ReadAddr
import Support.RunAppByAddr

dfu ::
    ( Monad m
    , MonadState Context m
    , LazyTransport t
    , MonadReader (D.Domain p) m
    ) =>
    Int -> (Word8, Word8) -> (OnMessage l -> m t) -> m ()
dfu address version transport' = do
    transport <- transport' onMessage
    info <- mkGetDfuInfo version transport
    addTask $ delay 10_000 "jump_to_firmware" $ jumpToFirmware $ fromIntegral address
    let
        onMessage buff _ = do
            action <- deref $ buff ! 0
            cond_
                [ action ==? actionGetInfo ==> onGetInfo info
                ]
    pure ()

jumpToFirmware :: Uint32 -> Ivory eff ()
jumpToFirmware address = do
    flag <- readAddr32u address
    when (flag /=? 0xff_ff_ff_ff) do
        disableIRQ
        setMSP =<< readAddr32u address
        runAppByAddr $ address + 4
