module Endpoint.DimmersSimple where

import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Data.Buffer
import Data.Record
import Data.Serialize
import GHC.TypeNats
import Ivory.Language
import Support.Cast

type DimmerStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { value :: IFloat
    ; synced :: IBool
    }
|]

data DimmersSimple n = DimmersSimple
    { dimmers :: Records n DimmerStruct
    , payload :: Buffer 3 Uint8
    , offset  :: Uint8
    }

mkDimmers ::
    forall n m.
    ( MonadState Context m
    , KnownNat n
    ) =>
    String ->
    Uint8 ->
    m (DimmersSimple n)
mkDimmers name offset = do
    addStruct (Proxy :: Proxy DimmerStruct)
    dimmers <-
        records'
            name
            [ value .= ival 0
            , synced .= ival false
            ]
    payload <- buffer "dimer_simple_message"
    pure DimmersSimple{dimmers, payload, offset}

message ::
    (KnownNat n) =>
    DimmersSimple n ->
    Uint8 ->
    Ivory eff (Buffer 3 Uint8)
message DimmersSimple{..} i = do
    pack payload 0 actionDim
    pack payload 1 $ i + 1 + offset
    pack payload 2 =<< castFloatToUint8 . (* 255) =<< deref (dimmer ~> value)
    pure payload
  where
    dimmer = dimmers ! toIx i

initialize ::
    Record DimmerStruct ->
    IFloat ->
    Ivory eff ()
initialize dimmer value' = do
    store (dimmer ~> value) value'

on :: (KnownNat n) => DimmersSimple n -> Uint8 -> Ivory eff ()
on DimmersSimple{..} i = do
    let dimmer' = dimmers ! toIx (i - offset)
    store (dimmer' ~> value) 1
    store (dimmer' ~> synced) false

off :: (KnownNat n) => DimmersSimple n -> Uint8 -> Ivory eff ()
off DimmersSimple{..} i = do
    let dimmer' = dimmers ! toIx (i - offset)
    store (dimmer' ~> value) 0
    store (dimmer' ~> synced) false

set :: (KnownNat n) => DimmersSimple n -> Uint8 -> IFloat -> Ivory eff ()
set DimmersSimple{..} i v = do
    let dimmer' = dimmers ! toIx (i - offset)
    store (dimmer' ~> value) v
    store (dimmer' ~> synced) false
