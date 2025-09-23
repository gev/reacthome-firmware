module Endpoint.Dimmers where

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

type DimmerStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { mode :: Uint8
    ; brightness :: IFloat
    ; velocity :: IFloat
    ; group :: Uint8
    ; value :: IFloat
    ; delta :: IFloat
    ; synced :: IBool
    }
|]

data Dimmers n = Dimmers
    { dimmers :: Records n DimmerStruct
    , payload :: Buffer 6 Uint8
    }

mkDimmers ::
    forall n m.
    ( MonadState Context m
    , KnownNat n
    ) =>
    String ->
    m (Dimmers n)
mkDimmers name = do
    addStruct (Proxy :: Proxy DimmerStruct)
    dimmers <-
        records name $
            go . fromIntegral
                <$> [1 .. natVal (aNat :: NatType n)]
    payload <- buffer "dimmer_message"
    pure Dimmers{dimmers, payload}
  where
    go i =
        [ mode .= ival 0
        , brightness .= ival 0
        , velocity .= ival 0
        , group .= ival i
        , value .= ival 0
        , delta .= ival 0
        , synced .= ival false
        ]

message ::
    (KnownNat n) =>
    Dimmers n ->
    Uint8 ->
    Ivory eff (Buffer 6 Uint8)
message Dimmers{..} i = do
    pack payload 0 actionDim
    pack payload 1 $ i + 1
    pack payload 2 =<< deref (dimmer ~> group)
    pack payload 3 =<< deref (dimmer ~> mode)
    pack payload 4 =<< castFloatToUint8 . (* 255) =<< deref (dimmer ~> brightness)
    pack payload 5 =<< castFloatToUint8 . (* 255) =<< deref (dimmer ~> velocity)
    pure payload
  where
    dimmer = dimmers ! toIx i

initialize ::
    Record DimmerStruct ->
    Uint8 ->
    Uint8 ->
    IFloat ->
    IFloat ->
    Ivory eff ()
initialize dimmer group' mode' brightness' velocity' = do
    store (dimmer ~> group) group'
    store (dimmer ~> mode) mode'
    store (dimmer ~> brightness) brightness'
    store (dimmer ~> value) brightness'
    store (dimmer ~> velocity) velocity'

on ::
    (KnownNat n) =>
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
on = runCheckMode $ \dimmer -> do
    store (dimmer ~> brightness) 1
    store (dimmer ~> value) 1

off :: (KnownNat n) => Dimmers n -> Uint8 -> Ivory eff ()
off = runCheckMode $ \dimmer -> do
    store (dimmer ~> brightness) 0
    store (dimmer ~> value) 0

fade ::
    (KnownNat n) =>
    IFloat ->
    IFloat ->
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
fade brightness' velocity' = runCheckMode $ \dimmer -> do
    mode <- deref $ dimmer ~> mode
    ifte_
        (mode ==? 4)
        ( ifte_
            (brightness' ==? 0)
            ( do
                store (dimmer ~> brightness) 0
                store (dimmer ~> value) 0
            )
            ( do
                store (dimmer ~> brightness) 1
                store (dimmer ~> value) 1
            )
        )
        ( do
            store (dimmer ~> brightness) brightness'
            store (dimmer ~> velocity) velocity'
            store (dimmer ~> delta) $ 0.0001 / (1.02 - velocity')
        )

setBrightness ::
    (KnownNat n) =>
    IFloat ->
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
setBrightness brightness' = runCheckMode $ \dimmer -> do
    mode <- deref $ dimmer ~> mode
    ifte_
        (mode ==? 4)
        ( ifte_
            (brightness' ==? 0)
            ( do
                store (dimmer ~> brightness) 0
                store (dimmer ~> value) 0
            )
            ( do
                store (dimmer ~> brightness) 1
                store (dimmer ~> value) 1
            )
        )
        ( do
            store (dimmer ~> brightness) brightness'
            store (dimmer ~> value) $ safeCast brightness'
        )

setMode ::
    (KnownNat n) =>
    Uint8 ->
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
setMode mode' = runDimmer $ \dimmer -> do
    store (dimmer ~> mode) mode'
    store (dimmer ~> brightness) 0
    store (dimmer ~> value) 0
    store (dimmer ~> synced) false

setGroup :: (KnownNat n) => Uint8 -> Dimmers n -> Uint8 -> Ivory eff ()
setGroup group' = runDimmer $ \dimmer -> do
    store (dimmer ~> group) group'
    store (dimmer ~> synced) false

runCheckMode ::
    (KnownNat n) =>
    (Record DimmerStruct -> Ivory eff ()) ->
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
runCheckMode run = runDimmer $ \dimmer -> do
    mode' <- deref $ dimmer ~> mode
    when (mode' /=? 0) $ do
        run dimmer
        store (dimmer ~> synced) false

runDimmer ::
    (KnownNat n) =>
    (Record DimmerStruct -> Ivory eff ()) ->
    Dimmers n ->
    Uint8 ->
    Ivory eff ()
runDimmer run Dimmers{..} index = do
    run dimmer
    syncDimmerGroup dimmers dimmer ix
  where
    ix = toIx index
    dimmer = dimmers ! ix

syncDimmerGroup ::
    (KnownNat n) =>
    Records n DimmerStruct ->
    Record DimmerStruct ->
    Ix n ->
    Ivory eff ()
syncDimmerGroup ds dimmer' ix' = do
    group' <- deref $ dimmer' ~> group
    arrayMap $ \ix'' ->
        when (ix'' /=? ix') $ do
            let dimmer'' = ds ! ix''
            group'' <- deref $ dimmer'' ~> group
            when (group'' ==? group') $ do
                let sync ::
                        (IvoryStore a) =>
                        Label DimmerStruct (Stored a) ->
                        Ivory eff ()
                    sync = copyLabel dimmer'' dimmer'
                sync velocity
                sync brightness
                sync mode
                sync value
                sync delta
                store (dimmer'' ~> synced) false

calculateValue :: Record DimmerStruct -> Ivory eff ()
calculateValue dimmer = do
    brightness' <- deref (dimmer ~> brightness)
    value' <- deref $ dimmer ~> value
    delta' <- deref $ dimmer ~> delta
    cond_
        [ value' <? brightness' ==> do
            let newValue = value' + delta'
            ifte_
                (newValue >? brightness')
                (store (dimmer ~> value) brightness')
                (store (dimmer ~> value) newValue)
        , value' >? brightness' ==> do
            let newValue = value' - delta'
            ifte_
                (newValue <? brightness')
                (store (dimmer ~> value) brightness')
                (store (dimmer ~> value) newValue)
        ]

copyLabel ::
    (IvoryStore a, IvoryStruct sym) =>
    Ref s1 (Struct sym) ->
    Ref s2 (Struct sym) ->
    Label sym ('Stored a) ->
    Ivory eff ()
copyLabel dst src label = store (dst ~> label) =<< deref (src ~> label)
