{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module Data.Display.FrameBuffer.NeoPixel.SPI where

import           Control.Monad.Writer     (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.Display.FrameBuffer
import           Data.Value
import           Interface.Display
import           Ivory.Language
import           Ivory.Language.Proxy

newtype FrameBufferNeoPixelSPI = FrameBufferNeoPixelSPI
    { runFrame :: RunValues Uint8
    }


neoPixelBufferSPI :: MonadWriter Context m
                  => String -> Int -> m FrameBufferNeoPixelSPI
neoPixelBufferSPI id size = do
    let size' = 3 * size
    let buff  = runValues_ (id <> "_frame_buffer_neo_pixel_spi") size'
    pure $ FrameBufferNeoPixelSPI buff



instance FrameBuffer FrameBufferNeoPixelSPI where
  clearByte = undefined
  writeByte = undefined
