{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}



module Data.Display.FrameBuffer.NeoPixel where

import           Control.Monad.State
import           Core.Context
import           Data.Display.FrameBuffer
import           Data.Value
import           GHC.TypeNats
import           Interface.Display
import           Ivory.Language
import           Ivory.Language.Proxy


data FrameBufferNeoPixel t = FrameBufferNeoPixel
    { runFrame :: RunValues t
    , zeroDuty :: t
    , oneDuty  :: t
    }


neoPixelBuffer :: forall m n t. (MonadState Context m, SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t)
                  => Uint8 -> String -> Int -> m (FrameBufferNeoPixel t)
neoPixelBuffer period id size = do
    let zeroDuty = safeCast $ period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    let size'    = 8 * size + 1 -- | add stop bit
    let runFrame = runValues (id <> "_frame_buffer_neo_pixel_pwm") $ replicate size' 0x0
    runFrame addArea
    pure $ FrameBufferNeoPixel { runFrame, zeroDuty, oneDuty }



instance IvoryStore t => FrameBuffer FrameBufferNeoPixel t where

  clearByte FrameBufferNeoPixel{..} i = do
    runFrame $ \frame -> for 8 $ \jx -> do
        let byte = addrOf frame ! (toIx (8 * i) + jx)
        store byte zeroDuty

  writeByte FrameBufferNeoPixel{..} i value = do
    v <- local $ ival value
    runFrame $ \frame -> for 8 $ \jx -> do
        v' <- deref v
        let b = v' .& 0x80
        let byte = addrOf frame ! (toIx (8 * i) + jx)
        ifte_ (b ==? 0x80)
            (store byte oneDuty )
            (store byte zeroDuty)
        store v $ v' `iShiftL` 1
