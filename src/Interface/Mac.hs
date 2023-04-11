{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Interface.Mac where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


type Mac = Values 6 Uint8


makeMac :: MonadWriter Context m
        => (Values 6 Uint8 -> forall s. Ivory (ProcEffects s ()) ())
        -> String
        -> m Mac
makeMac initMac name = do
    mac <- values_ name
    let initMac' :: Def ('[] ':-> ())
        initMac' = proc (name <> "_init") $ body $ initMac mac
    addInit initMac'
    pure mac
