module Feature.GetInfo where

import Control.Monad.Reader
import Core.Actions
import Core.Domain qualified as D
import Core.Meta
import Core.Transport
import Data.Char
import Data.Util
import Data.Word (Word8)
import Interface.MCU
import Ivory.Language

data GetInfo = forall t. (LazyTransport t) => GetInfo
    { status :: forall eff. Ivory eff Uint8
    , info :: [Uint8]
    , transport :: t
    }

mkGetInfo ::
    ( LazyTransport t
    , Monad m
    , MonadReader (D.Domain p i) m
    ) =>
    (forall eff. Ivory eff Uint8) -> (Word8, Word8) -> t -> m GetInfo
mkGetInfo status version transport = do
    meta <- asks D.meta
    let typeDevice = unPack16BE meta.model
        (major, minor) = version
        nameMcu = toEnum . ord . toLower <$> (meta.mcu.model <> meta.mcu.modification)
        info = fromIntegral <$> (typeDevice <> [meta.board, major, minor] <> nameMcu)
    pure GetInfo{transport, status, info}

mkGetMainInfo ::
    ( LazyTransport t
    , Monad m
    , MonadReader (D.Domain p i) m
    ) =>
    t -> m GetInfo
mkGetMainInfo transport = do
    meta <- asks D.meta
    mkGetInfo (pure 0) meta.version transport

mkGetDfuInfo ::
    ( LazyTransport t
    , Monad m
    , MonadReader (D.Domain p i) m
    ) =>
    (Word8, Word8) -> t -> m GetInfo
mkGetDfuInfo = mkGetInfo (pure 1)

onGetInfo :: GetInfo -> Ivory (ProcEffects s t) ()
onGetInfo GetInfo{..} = do
    let length' = 2 + fromIntegral (length info)
    lazyTransmit transport length' \transmit -> do
        transmit actionGetInfo
        transmit =<< status
        mapM_ transmit info