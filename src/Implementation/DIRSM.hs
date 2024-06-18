{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Implementation.DIRSM where

import           Control.Monad          hiding (when)
import           Core.Actions
import           Core.Controller
import           Data.Buffer
import           Data.Fixed             (List)
import           Data.Serialize
import           Data.Value
import qualified Feature.AOutputs       as FA
import           Feature.DInputs        (DInputs, forceSyncDInputs)
import           Feature.DS18B20
import           Feature.RS485.RSM      (forceSyncRSM', setMode, transmitRS485)
import           Feature.RS485.RSM.Data
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib



data DIRSM ni no nr = DIRSM { dinputs  :: DInputs ni
                            , rsm      :: List nr RSM
                            , aoutputs :: FA.AOutputs no
                            }



diRsm :: Monad m => m t
      -> (Bool -> t -> m (DInputs ni)) -> (t -> m (List nr RSM)) -> (t -> m (FA.AOutputs no)) -> (t -> m DS18B20)
      -> m (DIRSM ni no nr)
diRsm transport' dinputs' rsm' aoutputs' ds18b20 = do
      transport <- transport'
      ds18b20 transport
      dinputs  <- dinputs' True transport
      aoutputs <- aoutputs' transport
      rsm      <- rsm' transport
      pure DIRSM { dinputs, aoutputs, rsm}



instance (KnownNat ni, KnownNat no, KnownNat nr) => Controller (DIRSM ni no nr) where
    handle s@DIRSM{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo             ==> FA.onDo       aoutputs buff size
              , action ==? actionDim            ==> FA.onDim      aoutputs buff size
              , action ==? actionInitialize     ==> onInit        s        buff size
              , action ==? actionRs485Mode      ==> setMode       rsm      buff size
              , action ==? actionRs485Transmit  ==> transmitRS485 rsm      buff size
              , action ==? actionGetState       ==> onGetState    s
              ]


onInit :: (KnownNat l, KnownNat ni, KnownNat no, KnownNat nr)
       => DIRSM ni no nr -> Buffer l Uint8 -> Uint8
       -> Ivory (ProcEffects s t) ()
onInit DIRSM{..} buff size = undefined


onGetState DIRSM{..} = do
    forceSyncDInputs dinputs
    FA.forceSync     aoutputs
    forceSyncRSM'    rsm
