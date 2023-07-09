{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}

module Interface.Display where

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Ivory.Language


data Render d = Render
    { display   :: d
    , frameRate :: Uint32
    , render    :: forall s. Ivory (ProcEffects s ()) ()
    }


class Handler Render p => Display p f t | p -> f t where

    frameBuffer         :: MonadState Context m
                        => p -> String -> Int -> m (f t)

    transmitFrameBuffer :: p -> f t -> Ivory eff ()
