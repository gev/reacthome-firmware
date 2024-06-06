{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.DiRsm where

import           Control.Monad     hiding (when)
import           Core.Actions
import           Core.Controller
import           Data.Value
import           Feature.AOutputs
import           Feature.DInputs   (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data DIRSM ni no = DIRSM { dinputs    :: DInputs ni
                         , aoutputs   :: AOutputs no
                         }



diRsm :: Monad m => m t -> (Bool -> t -> m (DInputs ni)) -> (t -> m (AOutputs no)) -> (t -> m DS18B20) -> m (DIRSM ni no)
diRsm transport' dinputs' aoutputs' ds18b20 = do
      transport <- transport'
      ds18b20 transport
      dinputs  <- dinputs' True transport
      aoutputs <- aoutputs' transport
      pure DIRSM { dinputs, aoutputs }



instance (KnownNat ni, KnownNat no) => Controller (DIRSM ni no) where
    handle s@DIRSM{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo         ==> onDo   aoutputs buff size
              , action ==? actionDim        ==> onDim  aoutputs buff size
              , action ==? actionInitialize ==> onInit aoutputs buff size
              , action ==? actionGetState   ==> onGetState s
              ]


onGetState DIRSM{..} = do
    forceSyncDInputs dinputs
    forceSync aoutputs
