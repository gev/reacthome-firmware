{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Display.FrameBuffer.NeoPixel where

import           Control.Applicative    (Const (Const))
import           Control.Monad.State    (MonadState)
import           Core.Context
import           Data.Bits
import           Data.Buffer
import           Data.Functor
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.MemArea (ConstMemArea (ConstMemArea))
import           Ivory.Language.Proc
import           Ivory.Language.Proxy



data FrameBufferNeoPixel t = FrameBufferNeoPixel
    { matrix :: ConstRef Global (Array 256 (Array 8 (Stored t)))
    , buff   :: Buffer 12 t
    }



neoPixelBuffer :: ( MonadState Context m
                  , SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t
                  )
               => String -> Int -> m (FrameBufferNeoPixel t)
neoPixelBuffer id period = do
    let matrix = constArea "neo_pixel_matrix" . iarray $ iarray . map (ival . fromIntegral) <$> table
    buff      <- values'   ("neo_pixel_buffer" <> id) 0
    addConstArea matrix
    pure $ FrameBufferNeoPixel { matrix = addrOf matrix, buff }
    where
        table = [0..255] <&> \i ->
                    [0..7] <&> \j ->
                        let f = (i `shiftL` j) .&. 0x80 :: Int
                        in if f == 0x80 then oneDuty
                                        else zeroDuty
        zeroDuty = period `div` 4
        oneDuty  = 3 * zeroDuty


writeByte :: (IvoryType t, IvoryStore t) => FrameBufferNeoPixel t -> Uint8 -> Ivory eff ()
writeByte FrameBufferNeoPixel{..} value =
    arrayMap $ \ix ->
        store (buff ! toIx ix) =<< deref (matrix ! toIx value ! ix)
