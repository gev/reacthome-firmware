{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Display.FrameBuffer.NeoPixel where

import           Control.Applicative    (Const (Const))
import           Control.Monad.State    (MonadState)
import           Core.Context
import           Data.Bits
import           Data.Functor
import           Data.Value
import           GHC.IO.Buffer          (Buffer (Buffer))
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.MemArea (ConstMemArea (ConstMemArea))
import           Ivory.Language.Proc
import           Ivory.Language.Proxy



type BufferSize = 12

bufferSize :: Num a => a
bufferSize = fromIntegral $ fromTypeNat (aNat :: NatType BufferSize)


newtype FrameBufferNeoPixel t = FrameBufferNeoPixel
    { matrix :: ConstRef Global (Array 256 (Array BufferSize (Stored t)))
    }



neoPixelBuffer :: ( MonadState Context m
                  , SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t
                  )
               => String -> Int -> m (FrameBufferNeoPixel t)
neoPixelBuffer id period = do
    let matrix = constArea "neo_pixel_matrix" . iarray $ iarray . map (ival . fromIntegral) . (<> replicate (bufferSize - 8) 0) <$> table
    addConstArea matrix
    pure $ FrameBufferNeoPixel { matrix = addrOf matrix }
    where
        table = [0..255] <&> \i ->
                    [0..7] <&> \j ->
                        let f = (i `shiftL` j) .&. 0x80 :: Int
                        in if f == 0x80 then oneDuty
                                        else zeroDuty
        zeroDuty = period `div` 4
        oneDuty  = 3 * zeroDuty


getByte :: IvoryType t => FrameBufferNeoPixel t -> Uint8 -> ConstRef Global (Array BufferSize (Stored t))
getByte FrameBufferNeoPixel{..} value = matrix ! toIx value
