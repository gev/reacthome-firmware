{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Data.NeoPixel.Buffer.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Data.NeoPixel.Buffer
import           Data.Value
import           GHC.TypeNats
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy


data NeoPixelBufferPWM (n :: Nat) = NeoPixelBufferPWM
    { runFrame :: RunValues Uint8
    , zeroDuty :: Uint8
    , oneDuty  :: Uint8
    }


neoPixelBufferPWM :: forall m n. (MonadWriter Context m, KnownNat n)
                  => String -> Uint8 -> m (NeoPixelBufferPWM n)
neoPixelBufferPWM id period = do
    let zeroDuty = period `iDiv` 3
    let oneDuty  = 2 * zeroDuty
    let size     = fromInteger $ 8 * (21 + fromTypeNat (aNat :: NatType n))
    let runFrame = runValues (id <> "_neo_pixel_buffer_pwm") $ replicate size 0
    let npb      = NeoPixelBufferPWM { runFrame, zeroDuty, oneDuty }
    let initNeoPixelBufferPWM' :: Def ('[] :-> ())
        initNeoPixelBufferPWM' = proc (id <> "_neo_pixel_buffer_pwm_init") $ body $ do
            arrayMap $ \(ix :: Ix n) -> writeByte npb ix 127
    runFrame addArea
    addInit initNeoPixelBufferPWM'
    pure npb



instance NeoPixelBuffer NeoPixelBufferPWM where
  writeByte NeoPixelBufferPWM{..} ix value = do
    v <- local $ ival value
    times 8 $ \jx -> do
        s <- deref v
        let b = s .& 0x80
        runFrame $ \frame -> do
            let byte = addrOf frame ! toIx (ix * jx)
            ifte_ (b ==? 0x80)
                  (store byte oneDuty)
                  (store byte zeroDuty)
        store v $ s `iShiftL` 1
