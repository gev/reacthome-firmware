{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}



module Data.Display.FrameBuffer.NeoPixel.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Data.Display.FrameBuffer
import           Data.Value
import           GHC.TypeNats
import           Interface.Display
import           Ivory.Language
import           Ivory.Language.Proxy


data FrameBufferNeoPixelPWM = FrameBufferNeoPixelPWM
    { runFrame :: RunValues Uint8
    , zeroDuty :: Uint8
    , oneDuty  :: Uint8
    }


neoPixelBufferPWM :: forall m n. (MonadWriter Context m)
                  => Uint8 -> String -> Int -> m FrameBufferNeoPixelPWM
neoPixelBufferPWM period id size = do
    let zeroDuty = period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    let size'    = 8 * size + 1 -- | add stop bit
    let runFrame = runValues (id <> "_frame_buffer_neo_pixel_pwm") $ replicate size' 0x0
    runFrame addArea
    pure $ FrameBufferNeoPixelPWM { runFrame, zeroDuty, oneDuty }



instance FrameBuffer FrameBufferNeoPixelPWM where

  clearByte FrameBufferNeoPixelPWM{..} i = do
    runFrame $ \frame -> for 8 $ \jx -> do
        let byte = addrOf frame ! (toIx (8 * i) + jx)
        store byte zeroDuty

  writeByte FrameBufferNeoPixelPWM{..} i value = do
    v <- local $ ival value
    runFrame $ \frame -> for 8 $ \jx -> do
        v' <- deref v
        let b = v' .& 0x80
        let byte = addrOf frame ! (toIx (8 * i) + jx)
        ifte_ (b ==? 0x80)
            (store byte oneDuty )
            (store byte zeroDuty)
        store v $ v' `iShiftL` 1
