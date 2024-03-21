{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Display where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Value          (RunValues)
import           GHC.TypeNats
import           Ivory.Language


data Render d = Render
    { display   :: d
    , frameRate :: Uint32
    , runFrame  :: RunValues Uint8
    , render    :: forall s. Ivory (ProcEffects s ()) ()
    }


class Handler Render d => Display d

-- class Handler Render p => Display p f t | p -> f t where

--     frameBuffer         :: MonadState Context m
--                         => p -> String -> Int -> m (f t)

--     transmitFrameBuffer :: p -> f t -> Ivory eff ()
