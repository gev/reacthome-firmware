{-# LANGUAGE DataKinds             #-}
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


makeMac :: Monad m
    => (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> String
    -> WriterT Context m Mac
makeMac initMac name = do
    mac <- buffer name
    let initMac' :: Def ('[] ':-> ())
        initMac' = proc (name <> "_init") $ body $ initMac mac
    include initMac'
    pure mac
