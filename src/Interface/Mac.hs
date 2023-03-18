{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Interface.Mac where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


type Mac = Buffer 6 Uint8


makeMac :: MonadWriter Context m
        => (Buffer 6 Uint8 -> forall eff. Ivory eff ())
        -> String
        -> m Mac
makeMac initMac name = do
    mac <- buffer name
    let initMac' :: Def ('[] ':-> ())
        initMac' = proc (name <> "_init") $ body $ initMac mac
    addInit initMac'
    pure mac
