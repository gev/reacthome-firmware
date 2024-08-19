{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.ALED where

import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.State         (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain                 as D
import           Core.Handler
import           Core.Task
import qualified Core.Transport              as T
import           Data.Buffer
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.ALED               (brightness)
import qualified Endpoint.ALED               as E
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Display           (Display, Render (Render))
import           Interface.Mac
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast
import           Support.Device.GD32F3x0.FMC (FMC_FLAG)
import           Util.Random



data ALED ng ns np = forall d. Display d => ALED
    { display :: d
    , getALED :: E.ALED ng ns np
    }


maxValue = 0.3 :: IFloat

aled :: ( MonadState Context m
        , MonadReader (D.Domain p c) m
        , Display d, Handler (Render np) d
        , KnownNat ng, KnownNat ns, KnownNat np
        , T.Transport t
        ) => (p -> m d) -> t -> m (ALED ng ns np)
aled mkDisplay transport = do
    mcu         <- asks D.mcu
    display     <- mkDisplay $ peripherals mcu
    getALED     <- E.mkALED

    let aled = ALED { display, getALED }

    random <- mkRandom "aled" 1

    addInit "aled" $ configure aled

    addHandler $ Render display 2 (E.subPixels getALED) $ do
        update aled random

    pure aled



configure :: (KnownNat ng, KnownNat ns) => ALED ng ns np -> Ivory eff ()
configure ALED{..} = do
    let g = E.groups getALED
    let s = E.segments getALED

    store (g ! 0 ~> E.segmentNumber) 2
    store (g ! 0 ~> E.pixelSize) 3
    store (s ! 0 ~> E.size) 8
    store (s ! 0 ~> E.direction) true
    store (s ! 1 ~> E.size) 8
    store (s ! 1 ~> E.direction) false

    store (g ! 1 ~> E.segmentNumber) 1
    store (g ! 1 ~> E.pixelSize) 3
    store (s ! 2 ~> E.size) 8
    store (s ! 2 ~> E.direction) true


update :: forall s ng ns np. (KnownNat ng, KnownNat ns, KnownNat np)
       => ALED ng ns np -> Random Uint8 -> Ivory (ProcEffects s ()) ()
update ALED{..} random = do
    sx <- local (ival 0)
    px <- local (ival 0)
    arrayMap $ \gx -> do
        let g = E.groups getALED ! gx
        pixelSize' <- deref $ g ~> E.pixelSize
        segmentNumber' <- deref $ g ~> E.segmentNumber
        brightness' <- deref (g ~> brightness)
        for (toIx segmentNumber' :: Ix ns) $ \_ -> do
            sx' <- deref sx
            let s = E.segments getALED ! sx'
            segmentSize' <- deref $ s ~> E.size
            for (toIx segmentSize' :: Ix np) $ \_ -> do
                for (toIx pixelSize' :: Ix np) $ \_ -> do
                    px' <- deref px
                    let p = E.subPixels getALED
                    store (p ! px') . castDefault . (* brightness') . safeCast =<< next random
                    store px $ px' + 1
            store sx $ sx' + 1
