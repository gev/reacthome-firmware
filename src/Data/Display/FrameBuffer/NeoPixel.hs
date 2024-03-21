{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}



module Data.Display.FrameBuffer.NeoPixel where

import           Control.Monad.State
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Interface.Display
import           Ivory.Language


data FrameBufferNeoPixel t = FrameBufferNeoPixel
    { runFrame :: RunValues t
    , zeroDuty :: t
    , oneDuty  :: t
    }



neoPixelBuffer :: forall m n t. (MonadState Context m, SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t)
                  => Uint8 -> Int -> m (FrameBufferNeoPixel t)
neoPixelBuffer period size = do
    let zeroDuty = safeCast $ period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    let size'    = 8 * size + 1 -- | add stop bit
    let runFrame = runValues "frame_buffer_neo_pixel" $ replicate size' 0x0
    runFrame addArea
    pure $ FrameBufferNeoPixel { runFrame, zeroDuty, oneDuty }



writeByte :: IvoryStore t
          => FrameBufferNeoPixel t -> Sint32 -> Uint8
          -> Ivory ('Effects (Returns ()) b (Scope s)) ()
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
