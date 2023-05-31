{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Data.NeoPixel.Buffer.SPI where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.NeoPixel.Buffer
import           Data.Value
import           GHC.TypeNats
import           Interface.NeoPixel
import           Ivory.Language
import           Ivory.Language.Proxy

newtype NeoPixelBufferSPI (n :: Nat) = NeoPixelBufferSPI
    { runFrame :: RunValues Uint8
    }


neoPixelBufferSPI :: forall m n. (MonadWriter Context m, KnownNat n)
                  => String -> m (NeoPixelBufferSPI n)
neoPixelBufferSPI id = do
    let size = 3 * fromTypeNat (aNat :: NatType n)
    let buf  = runValues_ (id <> "_neo_pixel_buffer_spi") $ fromInteger size
    let npb  = NeoPixelBufferSPI buf
    let initNeoPixelBufferSPI' :: Def ('[] :-> ())
        initNeoPixelBufferSPI' = proc (id <> "_neo_pixel_buffer_spi_init") $ body $ do
            clearBuffer (npb :: NeoPixelBufferSPI n)
    addInit initNeoPixelBufferSPI'
    pure $ NeoPixelBufferSPI buf



instance NeoPixelBuffer NeoPixelBufferSPI where
  writeByte = undefined
