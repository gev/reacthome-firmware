{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Echo where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Controller
import           Core.Domain
import qualified Core.Domain          as D
import           Core.Feature
import qualified Core.Transport       as T
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib


data Echo = Echo
    { buff :: Buffer 10 Uint8
    , transmit :: forall n. KnownNat n
               => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



echo :: (MonadWriter Context m, MonadReader (Domain p t) m, T.Transport t) => m Feature
echo = do
    buff       <- buffer "echo"
    transport  <- asks D.transport
    pure . Feature $ Echo {buff, transmit = T.transmit transport}



instance Controller Echo where
    handle (Echo {..}) request n = do
        pure [ true ==> do
                arrayCopy buff request 0 (safeCast n)
                transmit buff
             ]
