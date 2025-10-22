{-# HLINT ignore "Use for_" #-}

module Feature.Touches where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Domain qualified as D
import Core.Task
import Core.Transport qualified as T
import Data.Buffer
import Data.Fixed as F
import Data.Index
import Data.Record
import Data.Serialize
import Data.Value
import Endpoint.DInputs qualified as DI
import GHC.TypeNats
import Interface.MCU (peripherals)
import Interface.Touch
import Interface.Touch qualified as I
import Ivory.Language
import Ivory.Stdlib

type LogBuffSize n = n * 2 + 2 

data Touches n = forall to. (I.Touch to) => Touches
    { getTouches :: List n to
    , getDInputs :: DI.DInputs n
    , currentTouch :: Value (Ix n)
    , indexTouch :: Value Uint8
    , buf :: Buffer (LogBuffSize n) Uint8
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s.
        Ivory (ProcEffects s ()) ()
    }

touches ::
    forall m n p c to tr.
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport tr
    , I.Touch to
    , KnownNat n, KnownNat (LogBuffSize n)
    ) =>
    IFloat ->
    IFloat ->
    List n (p -> IFloat -> IFloat -> m to) ->
    tr ->
    m (Touches n)
touches bottom top touches' transport = do
    mcu <- asks D.mcu
    ts <-
        mapM
            ( \touch ->
                touch
                    (peripherals mcu)
                    bottom
                    top
            )
            touches'
    currentTouch <- value "current_touches" 0
    indexTouch <- index "index_touches"
    dinputs <- DI.mkDinputs "touches"
    buf <- buffer "touch_buffer"

    let touches =
            Touches
                { getTouches = ts
                , getDInputs = dinputs
                , currentTouch
                , indexTouch
                , buf
                , transmit = T.transmitBuffer transport
                }

    -- addTask $ delay 50 "touches_log" $ sendTimeTask touches
    addTask $ delay 10 "touches_manage" $ manageTouches touches
    addTask $ yeld "touches_sync" $ syncTouches touches

    addSync "touches" $ forceSyncTouches touches

    pure touches

sendTimeTask ::
    (KnownNat n, KnownNat (LogBuffSize n)) =>
    Touches n ->
    Ivory (ProcEffects s ()) ()
sendTimeTask touches@Touches{..} = do
    shouldSend <- local $ ival false
    overSingleTouch touches \t i -> do
        let offset = fromIntegral $ i * 2 + 2
        prev <- unpackBE buf offset
        time <- castDefault @Sint16 <$> I.getDebug t
        when (time /=? prev) do
            packBE buf offset time
            store shouldSend true

    shouldSend' <- deref shouldSend
    when shouldSend' do
        store (buf ! 0) actionError
        store (buf ! 1) 1 -- type debug message
        transmit buf

overSingleTouch ::
    (KnownNat n, Monad m) =>
    Touches n ->
    (forall to. (I.Touch to) => to -> Integer -> m ()) ->
    m ()
overSingleTouch Touches{..} handle =
    zipWithM_ handle getTouches ints

forceSyncTouches :: (KnownNat n) => Touches n -> Ivory eff ()
forceSyncTouches Touches{..} =
    arrayMap \ix -> store ((DI.dinputs getDInputs ! ix) ~> DI.synced) false

manageTouches :: (KnownNat n) => Touches n -> Ivory eff ()
manageTouches Touches{..} =
    zipWithM_ zip getTouches ints
  where
    zip :: (I.Touch i) => i -> Int -> Ivory eff ()
    zip touch i = do
        let ix = fromIntegral i
        let dt = DI.dinputs getDInputs ! ix
        manageTouch dt touch

manageTouch ::
    (I.Touch i) =>
    Record DI.DInputStruct ->
    i ->
    Ivory eff ()
manageTouch di touch = do
    state0 <- deref $ di ~> DI.state
    state1 <- getState touch
    when (state1 /=? state0) do
        store (di ~> DI.state) state1
        store (di ~> DI.synced) false

syncTouches ::
    (KnownNat n) =>
    Touches n ->
    Ivory (ProcEffects s ()) ()
syncTouches touch@Touches{..} = do
    i <- deref indexTouch
    syncTouch touch i
    store indexTouch $ i + 1

syncTouch ::
    (KnownNat n) =>
    Touches n ->
    Uint8 ->
    Ivory (ProcEffects s ()) ()
syncTouch Touches{..} i = do
    let n = fromIntegral $ length getTouches
    let di = DI.dinputs getDInputs ! toIx i
    synced <- deref $ di ~> DI.synced
    when (iNot synced) do
        msg <- DI.message getDInputs (i .% n)
        transmit msg
        store (di ~> DI.synced) true
