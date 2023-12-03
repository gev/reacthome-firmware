{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Core.Dispatcher where

import           Core.Controller
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib

makeDispatcher :: (Controller c, KnownNat l)
               => [c]
               -> Buffer l Uint8
               -> Uint8
               -> forall s t. Ivory (ProcEffects s t) ()
makeDispatcher controllers buff n =
    when (n >? 0) $ cond_ =<< conditions
    where
        conditions = do
            c <- traverse run handlers
            pure $ concat c
        handlers = handle <$> controllers
        run h    = h buff n
