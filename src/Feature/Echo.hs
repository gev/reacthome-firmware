{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
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


newtype Echo = Echo
    { transmit :: forall n s. KnownNat n
               => Buffer n Uint8
               -> Ivory (ProcEffects s ()) ()
    }


echo :: ( MonadWriter Context m
        , MonadReader (Domain p t) m
        , T.Transport t
        ) => m Feature
echo = do
    transport  <- asks D.transport
    pure . Feature $ Echo { transmit = T.transmit transport }


instance Controller Echo where
    handle (Echo {..}) buff n = do
        pure [ true ==> transmit buff
             ]
