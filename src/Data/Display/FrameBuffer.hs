{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Display.FrameBuffer where

import           GHC.TypeNats
import           Ivory.Language



class FrameBuffer f t where
    clearByte   :: f t
                -> Sint32
                -> Ivory eff ()

    writeByte   :: f t
                -> Sint32
                -> Uint8
                -> Ivory ('Effects (Returns ()) b (Scope s)) ()
