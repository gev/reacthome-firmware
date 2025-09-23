{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoint.AOutputs where

import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Data.Buffer
import Data.Record
import Data.Serialize
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Proxy (NatType, aNat)
import Ivory.Stdlib
import Support.Cast

type AOutputStruct = "aoutput_struct"

[ivory|
    struct aoutput_struct
    { value :: IFloat
    ; synced :: IBool
    }
|]

data AOutputs n = AOutputs
    { aoutputs :: Records n AOutputStruct
    , payload :: Buffer 3 Uint8
    }

mkAOutputs ::
    forall n m.
    ( MonadState Context m
    , KnownNat n
    ) =>
    String ->
    m (AOutputs n)
mkAOutputs name = do
    addStruct (Proxy :: Proxy AOutputStruct)
    aoutputs <-
        records'
            name
            [ value .= ival 0
            , synced .= ival false
            ]
    payload <- buffer "aoutput_message"
    pure AOutputs{aoutputs, payload}

message ::
    (KnownNat n) =>
    AOutputs n ->
    Uint8 ->
    Ivory eff (Buffer 3 Uint8)
message AOutputs{..} i = do
    pack payload 0 actionDim
    pack payload 1 $ i + 1
    pack payload 2 =<< castFloatToUint8 . (* 255) =<< deref (aoutput ~> value)
    pure payload
  where
    aoutput = aoutputs ! toIx i

initialize ::
    Record AOutputStruct ->
    IFloat ->
    Ivory eff ()
initialize aoutput value' = do
    store (aoutput ~> value) value'

on :: (KnownNat n) => AOutputs n -> Uint8 -> Ivory eff ()
on AOutputs{..} i = do
    let aoutput' = aoutputs ! toIx i
    store (aoutput' ~> value) 1
    store (aoutput' ~> synced) false

off :: (KnownNat n) => AOutputs n -> Uint8 -> Ivory eff ()
off AOutputs{..} i = do
    let aoutput' = aoutputs ! toIx i
    store (aoutput' ~> value) 0
    store (aoutput' ~> synced) false

set :: (KnownNat n) => AOutputs n -> Uint8 -> IFloat -> Ivory eff ()
set AOutputs{..} i v = do
    let aoutput' = aoutputs ! toIx i
    store (aoutput' ~> value) v
    store (aoutput' ~> synced) false
