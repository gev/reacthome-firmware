{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module Data.NeoPixel.Buffer.SPI where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.NeoPixel.Buffer
import           Data.Value
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy

newtype NeoPixelBufferSPI = NeoPixelBufferSPI
    { runFrame :: RunValues Uint8
    }


neoPixelBufferSPI :: MonadWriter Context m
                  => String -> Int -> m NeoPixelBufferSPI
neoPixelBufferSPI id size = do
    let size' = 3 * size
    let buf   = runValues_ (id <> "_neo_pixel_buffer_spi") size'
    let npb   = NeoPixelBufferSPI buf
    pure $ NeoPixelBufferSPI buf



instance NeoPixelBuffer NeoPixelBufferSPI where
  clearByte = undefined
  writeByte = undefined
