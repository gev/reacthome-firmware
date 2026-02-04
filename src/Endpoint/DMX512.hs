module Endpoint.DMX512 where

import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Data.Buffer
import Data.Record
import Data.Serialize
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Support.Cast

type Dmx512Struct = "dmx512_struct"

[ivory|
    struct dmx512_struct
    { brightness :: IFloat
    ; velocity :: IFloat
    ; value :: IFloat
    ; delta :: IFloat
    ; synced :: IBool
    }
|]

data DMX512 n = DMX512
    { dmx512 :: Records n Dmx512Struct
    , port :: Uint8
    , payload :: Buffer 6 Uint8
    }

mkDMX512 ::
    forall n m.
    ( MonadState Context m
    , KnownNat n
    ) =>
    String ->
    Int ->
    m (DMX512 n)
mkDMX512 name port = do
    addStruct (Proxy :: Proxy Dmx512Struct)
    dmx512 <-
        records' name initVal
    payload <- buffer "dmx_message"
    pure DMX512{dmx512, port = fromIntegral port, payload}
  where
    initVal =
        [ brightness .= ival 0
        , velocity .= ival 0
        , value .= ival 0
        , delta .= ival 0
        , synced .= ival false
        ]

message ::
    (KnownNat n) =>
    DMX512 n ->
    Uint16 ->
    Ivory eff (Buffer 6 Uint8)
message DMX512{..} i = do
    pack payload 0 actionDMX512
    pack payload 1 port
    packBE payload 2 (i + 1)
    pack payload 4 =<< castFloatToUint8 . (* 255) =<< deref (dmx ~> brightness)
    pack payload 5 =<< castFloatToUint8 . (* 255) =<< deref (dmx ~> velocity)
    pure payload
  where
    dmx = dmx512 ! toIx i

initialize ::
    Record Dmx512Struct ->
    Ivory eff ()
initialize dmx = do
    store (dmx ~> brightness) 0
    store (dmx ~> value) 0
    store (dmx ~> velocity) 0

on ::
    (KnownNat n) =>
    DMX512 n ->
    Uint16 ->
    Ivory eff ()
on DMX512{..} index = do
    store (dmx ~> brightness) 1
    store (dmx ~> value) 1
    store (dmx ~> synced) false
  where
    dmx = dmx512 ! toIx index

off :: (KnownNat n) => DMX512 n -> Uint16 -> Ivory eff ()
off DMX512{..} index = do
    store (dmx ~> brightness) 0
    store (dmx ~> value) 0
    store (dmx ~> synced) false
  where
    dmx = dmx512 ! toIx index

fade ::
    (KnownNat n) =>
    DMX512 n ->
    IFloat ->
    IFloat ->
    Uint16 ->
    Ivory eff ()
fade DMX512{..} brightness' velocity' index = do
    store (dmx ~> brightness) brightness'
    store (dmx ~> velocity) velocity'
    store (dmx ~> delta) $ 0.0001 / (1.02 - velocity')
    store (dmx ~> synced) false
  where
    dmx = dmx512 ! toIx index

setBrightness ::
    (KnownNat n) =>
    DMX512 n ->
    IFloat ->
    Uint16 ->
    Ivory eff ()
setBrightness DMX512{..} brightness' index = do
    store (dmx ~> brightness) brightness'
    store (dmx ~> value) $ safeCast brightness'
    store (dmx ~> synced) false
  where
    dmx = dmx512 ! toIx index

calculateValue :: Record Dmx512Struct -> Ivory eff ()
calculateValue dmx = do
    brightness' <- deref (dmx ~> brightness)
    value' <- deref $ dmx ~> value
    delta' <- deref $ dmx ~> delta
    cond_
        [ value' <? brightness' ==> do
            let newValue = value' + delta'
            ifte_
                (newValue >? brightness')
                do store (dmx ~> value) brightness'
                do store (dmx ~> value) newValue
        , value' >? brightness' ==> do
            let newValue = value' - delta'
            ifte_
                (newValue <? brightness')
                do store (dmx ~> value) brightness'
                do store (dmx ~> value) newValue
        ]