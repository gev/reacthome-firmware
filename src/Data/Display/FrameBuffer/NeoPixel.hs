{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE NoStarIsType     #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Display.FrameBuffer.NeoPixel where

import           Control.Monad.State
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Interface.Display
import           Ivory.Language
import           Ivory.Language.Proc



type BufferSize n = 8 * n + 1



data FrameBufferNeoPixel n t = FrameBufferNeoPixel
    { buffer   :: Values (BufferSize n) t
    , zeroDuty :: t
    , oneDuty  :: t
    }



neoPixelBuffer :: (MonadState Context m, SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t, KnownNat (BufferSize n))
               => Uint8 -> m (FrameBufferNeoPixel n t)
neoPixelBuffer period = do
    let zeroDuty = safeCast $ period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    buffer      <- values' "neo_pixel_buffer" 0x0
    pure $ FrameBufferNeoPixel { buffer, zeroDuty, oneDuty }



writeByte :: (IvoryStore t, KnownNat (BufferSize n))
          => FrameBufferNeoPixel n t -> Sint32 -> Uint8
          -> Ivory ('Effects (Returns ()) b (Scope s)) ()
writeByte FrameBufferNeoPixel{..} i value = do
    v <- local $ ival value
    for 8 $ \jx -> do
        v' <- deref v
        let b = v' .& 0x80
        let byte = buffer ! (toIx (8 * i) + jx)
        ifte_ (b ==? 0x80)
            (store byte oneDuty )
            (store byte zeroDuty)
        store v $ v' `iShiftL` 1
