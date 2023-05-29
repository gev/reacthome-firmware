{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}


module Data.NeoPixel.Buffer.PWM where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy

data NeoPixelBufferPWM n = NeoPixelBufferPWM
    { frame    :: Buffer (n * 8) Uint8
    , zeroDuty :: Uint8
    , oneDuty  :: Uint8
    }


neoPixelBufferPWM :: (MonadWriter Context m, KnownNat (n * 8), KnownNat (n * 1))
                  => String -> Uint8 -> m (NeoPixelBufferPWM n)
neoPixelBufferPWM id period = do
    buf     <- buffer id
    let zero = period / 3
    let one  = 2 * zero
    let npb  = NeoPixelBufferPWM buf zero one
    let initNeoPixelBufferPWM' :: Def ('[] :-> ())
        initNeoPixelBufferPWM' = proc "neo_pixel_buffer_pwm_init" $ body $ do
            arrayMap $ \ix -> writeByte npb ix 0
    addInit initNeoPixelBufferPWM'
    pure npb



instance NeoPixelBuffer n NeoPixelBufferPWM where
  writeByte NeoPixelBufferPWM{..} ix v =
    for 8 $ \jx -> do
