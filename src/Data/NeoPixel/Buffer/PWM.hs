{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}



module Data.NeoPixel.Buffer.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Data.NeoPixel.Buffer
import           Data.Value
import           GHC.TypeNats
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy


data NeoPixelBufferPWM = NeoPixelBufferPWM
    { runFrame :: RunValues Uint8
    , zeroDuty :: Uint8
    , oneDuty  :: Uint8
    }


neoPixelBufferPWM :: forall m n. (MonadWriter Context m)
                  => Uint8 -> String -> Int -> m NeoPixelBufferPWM
neoPixelBufferPWM period id size = do
    let zeroDuty = period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    let size'    = 8 * size + 1 -- | add stop bit
    let runFrame = runValues (id <> "_neo_pixel_buffer_pwm") $ replicate size' 0x0
    runFrame addArea
    pure $ NeoPixelBufferPWM { runFrame, zeroDuty, oneDuty }



instance NeoPixelBuffer NeoPixelBufferPWM where

  clearByte NeoPixelBufferPWM{..} i = do
    runFrame $ \frame -> for 8 $ \jx -> do
        let byte = addrOf frame ! (toIx i + jx)
        store byte zeroDuty

  writeByte NeoPixelBufferPWM{..} i value = do
    v <- local $ ival value
    runFrame $ \frame -> for 8 $ \jx -> do
        v' <- deref v
        let b = v' .& 0x80
        let byte = addrOf frame ! (toIx i + jx)
        ifte_ (b ==? 0x80)
            (store byte oneDuty )
            (store byte zeroDuty)
        store v $ v' `iShiftL` 1
