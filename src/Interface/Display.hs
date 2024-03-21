{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Display where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


data Render d = forall n. KnownNat n => Render
    { display   :: d
    , frameRate :: Uint32
    , frame     :: Buffer n Uint8
    , render    :: forall s. Ivory (ProcEffects s ()) ()
    }


-- class Handler Render p => Display p f t | p -> f t where

--     frameBuffer         :: MonadState Context m
--                         => p -> String -> Int -> m (f t)

--     transmitFrameBuffer :: p -> f t -> Ivory eff ()
