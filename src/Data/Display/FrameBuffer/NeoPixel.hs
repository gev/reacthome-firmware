-- {-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE NamedFieldPuns      #-}
-- {-# LANGUAGE RecordWildCards     #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Data.Display.FrameBuffer.NeoPixel where

-- import           Control.Applicative    (Const (Const))
-- import           Control.Monad.State    (MonadState)
-- import           Core.Context
-- import           Data.Bits
-- import           Data.Buffer
-- import           Data.Functor
-- import           Data.Value
-- import           GHC.TypeNats
-- import           Ivory.Language
-- import           Ivory.Language.MemArea (ConstMemArea (ConstMemArea))
-- import           Ivory.Language.Proc
-- import           Ivory.Language.Proxy



-- data FrameBufferNeoPixel t = FrameBufferNeoPixel
--     { matrix :: ConstRef Global (Array 256 (Array 8 (Stored t)))
--     , buff   :: Buffer 12 t
--     }



-- neoPixelBuffer :: ( MonadState Context m
--                   , SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t
--                   )
--                => String -> Int -> m (FrameBufferNeoPixel t)
-- neoPixelBuffer id period = do
--     let matrix = constArea "neo_pixel_matrix" . iarray $ iarray . map (ival . fromIntegral) <$> table
--     buff      <- values'   ("neo_pixel_buffer" <> id) 0
--     addConstArea matrix
--     pure $ FrameBufferNeoPixel { matrix = addrOf matrix, buff }
--     where
--         table = [0..255] <&> \i ->
--                     [0..7] <&> \j ->
--                         let f = (i `shiftL` j) .&. 0x80 :: Int
--                         in if f == 0x80 then oneDuty
--                                         else zeroDuty
--         zeroDuty = period `div` 4
--         oneDuty  = 3 * zeroDuty


-- writeByte :: (IvoryType t, IvoryStore t) => FrameBufferNeoPixel t -> Uint8 -> Ivory eff ()
-- writeByte FrameBufferNeoPixel{..} value =
--     arrayMap $ \ix ->
--         store (buff ! toIx ix) =<< deref (matrix ! toIx value ! ix)


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



data FrameBufferNeoPixel t = FrameBufferNeoPixel
    { buff     :: Values 12 t
    , zeroDuty :: t
    , oneDuty  :: t
    }



neoPixelBuffer :: ( MonadState Context m
                  , SafeCast Uint8 t, IvoryInit t, IvoryZeroVal t, Num t
                  )
               => String -> Uint8 -> m (FrameBufferNeoPixel t)
neoPixelBuffer id period = do
    let zeroDuty = safeCast $ period `iDiv` 4
    let oneDuty  = 3 * zeroDuty
    buff        <- values' ("neo_pixel_buffer" <> id) 0x0
    pure $ FrameBufferNeoPixel { buff, zeroDuty, oneDuty }



writeByte :: IvoryStore t
          => FrameBufferNeoPixel t -> Uint8
          -> Ivory ('Effects (Returns ()) b (Scope s)) ()
writeByte FrameBufferNeoPixel{..} value = do
    v <- local $ ival value
    for 8 $ \jx -> do
        v' <- deref v
        let b = v' .& 0x80
        let byte = buff ! jx
        ifte_ (b ==? 0x80)
            (store byte oneDuty )
            (store byte zeroDuty)
        store v $ v' `iShiftL` 1
