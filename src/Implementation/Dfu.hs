module Implementation.Dfu where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Meta (Meta (..))
import Core.Task (delay)
import Core.Transport
import Data.Buffer (Buffer)
import Data.Serialize (Serialize (unpack), SerializeLE (unpackLE), unpackBE)
import Data.Value (Value, value)
import Data.Word
import Feature.GetInfo
import GHC.TypeNats (KnownNat)
import Interface.Flash qualified as F
import Interface.MCU (mcuName)
import Ivory.Language
import Ivory.Stdlib
import Support.CMSIS.CoreCM4 (nvicSystemReset)
import Support.CMSIS.CoreCMFunc
import Support.ReadAddr
import Support.RunAppByAddr

data DFU p = forall f t. (F.Flash f, LazyTransport t) => DFU
    { meta :: Meta p
    , version :: (Word8, Word8)
    , info :: GetInfo
    , numberOfChunks :: Value Uint16
    , currentChunk :: Value Uint16
    , shouldRepeatRequest :: Value IBool
    , firmwareAddress :: Uint32
    , transport :: t
    , mem :: f
    }

dfu ::
    ( Monad m
    , MonadState Context m
    , F.Flash f
    , LazyTransport t
    , MonadReader (D.Domain p i) m
    ) =>
    Int -> (Word8, Word8) -> f -> m t -> m (DFU p)
dfu address version mem transport' = do
    let firmwareAddress = fromIntegral address
    meta <- asks D.meta
    transport <- transport'
    info <- mkGetDfuInfo version transport

    numberOfChunks <- value "number_of_chunks" 0
    currentChunk <- value "current_chunk" 0
    shouldRepeatRequest <- value "should_repeat_request" false

    let dfu = DFU{..}

    addTask $ delay 10_000 "jump_to_firmware" $ jumpToFirmware dfu
    addTask $ delay 2_000 "repeat_chunk_request" $ repeatChunkRequest dfu
    pure dfu

jumpToFirmware :: DFU p -> Ivory eff ()
jumpToFirmware DFU{..} = do
    flag <- readAddr32u firmwareAddress
    when (flag /=? 0xff_ff_ff_ff) do
        disableIRQ
        setMSP =<< readAddr32u firmwareAddress
        runAppByAddr $ firmwareAddress + 4

repeatChunkRequest :: DFU p -> Ivory (ProcEffects s t) ()
repeatChunkRequest dfu@DFU{..} = do
    shouldRepeatRequest' <- deref shouldRepeatRequest
    when shouldRepeatRequest' do
        requestChunk dfu =<< deref currentChunk

onUpdateFirmware ::
    (KnownNat l) =>
    DFU p ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onUpdateFirmware dfu buff size =
    cond_
        [ size ==? 1 ==> nvicSystemReset
        , size >=? 8 ==> do
            index <- unpackBE @Uint16 buff 1
            ifte_
                (index ==? 0)
                do receiveHeader dfu buff size
                do receiveChunk dfu buff size index
        ]

{-
    Header format:
    fb     | 0000  | 0345   | 0020   | 03      | 040a     | 0001    | 67643332663333306b387536
    action | index | chunks | device | board   | firmware | dfu     | mcu name
           |       | number | number | version | version  | version |
    0      | 1     | 3      | 5      | 7       | 8        | 10      | 12
-}
receiveHeader ::
    (KnownNat l) =>
    DFU p ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
receiveHeader dfu@DFU{..} buff size =
    when (size ==? fromIntegral (12 + length name)) do
        store shouldRepeatRequest false
        deviceType <- unpackBE @Uint16 buff 5
        boardVersion <- unpack @Uint8 buff 7
        dfuMajorVersion <- unpack @Uint8 buff 10
        when
            ( deviceType
                ==? fromIntegral meta.model
                .&& boardVersion
                ==? fromIntegral meta.board
                .&& dfuMajorVersion
                ==? fromIntegral (fst version)
            )
            do
                sameMcu <- checkMcu name 12 true
                when sameMcu do
                    cleanPage mem $ F.Addr firmwareAddress
                    store numberOfChunks =<< unpackBE buff 3
                    store currentChunk 2
                    requestChunk dfu 2
                    store shouldRepeatRequest true
  where
    checkMcu [] _ same = pure same
    checkMcu (n : ns) ix same = do
        m <- deref $ buff ! ix
        checkMcu ns (ix + 1) (same .&& n ==? m)

    name = fromIntegral . fromEnum <$> mcuName meta.mcu

{-
    Chunk format:
    fb     | 0001  | 08002000 | 20020020894c0008d54c0008d54c0008
    action | index | adress   | data
    0      | 1     | 3        | 7
-}
receiveChunk ::
    (KnownNat l) =>
    DFU p ->
    Buffer l Uint8 ->
    Uint8 ->
    Uint16 ->
    Ivory (ProcEffects s t) ()
receiveChunk dfu@DFU{..} buff size index = do
    currentChunk' <- deref currentChunk
    when (currentChunk' ==? index) do
        store shouldRepeatRequest false
        address <- F.Addr <$> unpackBE @Uint32 buff 3
        ifte_
            (index ==? 1)
            do
                writeChunk mem address buff size
            do
                cleanPage mem address
                writeChunk mem address buff size
                numberOfChunks' <- deref numberOfChunks
                next <- ifte
                    (index ==? numberOfChunks')
                    do pure 1
                    do pure $ index + 1
                store currentChunk next
                requestChunk dfu next
                store shouldRepeatRequest true

writeChunk ::
    (KnownNat l, F.Flash f) =>
    f ->
    F.Addr ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
writeChunk mem address buff size = do
    let n = (size - 7) `iDiv` 4
    for (toIx n) \ix -> do
        let offset = address + F.Addr (safeCast ix * 4)
        word <- unpackLE buff (ix * 4 + 7)
        F.write mem offset word

cleanPage :: (F.Flash f) => f -> F.Addr -> Ivory (ProcEffects s t) ()
cleanPage mem address =
    when (F.getAddr address .% 0x400 ==? 0) do
        F.erasePage mem address

requestChunk :: DFU p -> Uint16 -> Ivory (ProcEffects s t) ()
requestChunk DFU{..} chunk =
    lazyTransmit transport 3 \transmit ->
        mapM_ transmit [actionUpdateFirmware, ubits chunk, lbits chunk]

instance Controller (DFU p) where
    handle dfu@DFU{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionGetInfo ==> onGetInfo info
            , action ==? actionUpdateFirmware ==> onUpdateFirmware dfu buff size
            ]
