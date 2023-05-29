{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}


module Data.NeoPixel.Buffer.SPI where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.NeoPixel.Buffer
import           GHC.TypeNats
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy

newtype NeoPixelBufferSPI n = NeoPixelBufferSPI
    { frame :: Buffer (n * 4) Uint8
    }


neoPixelBufferSPI :: (MonadWriter Context m, KnownNat (n * 4), KnownNat (n * 1))
                  => String -> m (NeoPixelBufferSPI n)
neoPixelBufferSPI id = do
    buf     <- buffer id
    let npb  = NeoPixelBufferSPI buf
    let initNeoPixelBufferSPI' :: Def ('[] :-> ())
        initNeoPixelBufferSPI' = proc "neo_pixel_buffer_spi_init" $ body $ do
            arrayMap $ \ix -> writeByte npb ix 0
    addInit initNeoPixelBufferSPI'
    pure $ NeoPixelBufferSPI buf



instance NeoPixelBuffer n NeoPixelBufferSPI where
  writeByte = undefined
