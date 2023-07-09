{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}


module Data.Display.FrameBuffer.NeoPixel.SPI where

import           Control.Monad.State      (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Display.FrameBuffer
import           Data.Value
import           Interface.Display
import           Ivory.Language
import           Ivory.Language.Proxy

newtype FrameBufferNeoPixelSPI t = FrameBufferNeoPixelSPI
    { runFrame :: RunValues t
    }


neoPixelBufferSPI :: (MonadState Context m, IvoryInit t, IvoryZeroVal t)
                  => String -> Int -> m (FrameBufferNeoPixelSPI t)
neoPixelBufferSPI id size = do
    let size' = 3 * size
    let buff  = runValues_ (id <> "_frame_buffer_neo_pixel_spi") size'
    pure $ FrameBufferNeoPixelSPI buff



instance FrameBuffer FrameBufferNeoPixelSPI t where
  clearByte = undefined
  writeByte = undefined
