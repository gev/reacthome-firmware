{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top.Buttons where

import           Control.Monad.State    (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Transport
import           Data.Buffer
import           Data.Color
import           Data.Serialize
import           Data.Value
import           Endpoint.DInputs       (DInputs (dinputs), state)
import           Feature.Smart.Top.LEDs (LEDs (order, pixels))
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data Buttons n l = forall t. (LazyTransport t, KnownNat l) => Buttons
    { leds            :: LEDs    l
    , getDInputs      :: DInputs n
    , leds'per'button :: Ix      l
    , start           :: Value   IBool
    , findMe          :: Value   IBool
    , t               :: Value   Sint32
    , transport       :: t
    }



mkButtons :: (MonadState Context m, LazyTransport t, KnownNat l)
          => LEDs l -> DInputs n -> Ix l -> t -> m (Buttons n l)
mkButtons leds getDInputs leds'per'button transport = do
    start      <- value "buttons_start"   true
    findMe     <- value "buttons_find_me" false
    t          <- value "buttons_t"       0

    addConstArea sinT

    pure Buttons { leds, getDInputs, leds'per'button
                 , start, findMe, t
                 , transport
                 }


updateButtons :: (KnownNat l, KnownNat n)
              => Buttons n l
              -> Ivory (ProcEffects s ()) ()
updateButtons Buttons{..} = do
    pixel'  <- local . istruct $ rgb 0 0 0
    pixel'' <- local . istruct $ rgb 1 1 1
    start'  <- deref start
    when start'
        (do
            t' <- deref t
            v  <- deref $ addrOf sinT ! toIx t'
            pixel <- local . istruct $ hsv v v v
            hsv'to'rgb pixel pixel'
            findMe' <- deref findMe
            when (t' ==? 120 .&& iNot findMe')
                 (store start false)
            store t $ t' + 1
        )

    arrayMap $ \ix -> do
        state' <- deref $ dinputs getDInputs ! ix ~> state
        for leds'per'button $ \ kx -> do
            let sx = toIx $ fromIx leds'per'button * fromIx ix + fromIx kx
            dx <- deref $ order leds ! sx
            ifte_ state'
                (run (pixels leds ! dx) pixel'')
                (when start'
                    (run (pixels leds ! dx) pixel')
                )
    where
        run dst src = do
            store (dst ~> r) =<< deref (src ~> r)
            store (dst ~> g) =<< deref (src ~> g)
            store (dst ~> b) =<< deref (src ~> b)


onFindMe :: KnownNat b
         => Buttons n l
         -> Buffer b Uint8
         -> Uint8
         -> Ivory (ProcEffects s t) ()
onFindMe Buttons{..} buff size =
    when (size ==? 2) $ do
        v <- unpack buff 1
        store findMe v
        store t 0
        store start true
        lazyTransmit transport 2 $ \transmit -> do
            transmit actionFindMe
            transmit $ safeCast v



sinT :: ConstMemArea (Array 120 (Stored IFloat))
sinT = constArea "sinT" $ iarray $ ival . ifloat . f . fromInteger <$> [0..119]
    where f i = sin (pi * i / 120)
