{-# LANGUAGE RankNTypes #-}

module Core.Dispatcher where

import           Core.Controller
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib

makeDispatcher :: (Controller c, KnownNat l, IvoryStore t, IvoryEq t, IvoryOrd t, Num t)
               => [c]
               -> Buffer l t
               -> n
               -> forall s. Ivory (ProcEffects s ()) ()
makeDispatcher controllers buff n = cond_ =<< conditions
    where
        conditions = do
            c <- traverse run handlers
            pure $ concat c
        handlers   = handle <$> controllers
        run h      = h buff n
