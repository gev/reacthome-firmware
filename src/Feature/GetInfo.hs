module Feature.GetInfo where

import Control.Monad.Reader
import Core.Actions
import Core.Domain qualified as D
import Core.Meta
import Core.Transport
import Data.Char
import Data.Util
import Interface.MCU
import Ivory.Language

data GetInfo = forall t. (LazyTransport t) => GetInfo
    { transport :: t
    , info :: [Uint8]
    }

mkGetInfo ::
    ( LazyTransport t
    , Monad m
    , MonadReader (D.Domain p i) m
    ) =>
    t -> m GetInfo
mkGetInfo transport = do
    meta <- asks D.meta
    let typeDevice = fromIntegral <$> unPack16BE (fromIntegral meta.model) :: [Uint8]
        major = fst meta.version
        minor = snd meta.version
        nameMcu = toEnum . ord . toLower <$> (meta.mcu.model <> meta.mcu.modification)
        info = actionGetInfo : typeDevice <> (fromIntegral <$> (meta.board : major : minor : nameMcu))

    pure GetInfo{transport, info}

onGetInfo :: GetInfo -> Ivory (ProcEffects s t) ()
onGetInfo GetInfo{..} = do
    let length' = fromIntegral $ length info
    lazyTransmit transport length' (`mapM_` info)