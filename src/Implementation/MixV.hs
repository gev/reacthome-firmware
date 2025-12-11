module Implementation.MixV where

import Control.Monad.Reader hiding (local)
import Core.Actions
import Core.Controller
import Core.Domain qualified as D
import Data.Buffer
import Data.Value
import Endpoint.AOutputs qualified as EAO
import Endpoint.DimmersSimple qualified as EDIM
import Feature.AOutputs qualified as AO
import Feature.DInputs
import Feature.DS18B20
import Feature.DimmersSimple qualified as DIM
import GHC.TypeLits
import Ivory.Language
import Ivory.Language.Proxy (NatType, aNat)
import Ivory.Stdlib
import Data.Serialize (unpack)

data Mix ni nd no = Mix
    { dinputs :: DInputs ni
    , dimmers :: DIM.DimmersSimple nd
    , aoutputs :: AO.AOutputs no
    , shouldInit :: Value IBool
    }

mix :: forall no ni nd m p c t.
    (KnownNat no, Monad m, MonadReader (D.Domain p c) m) =>
    m t ->
    (Bool -> t -> m (DInputs ni)) ->
    (t -> m (AO.AOutputs no)) ->
    (Uint8 -> t -> m (DIM.DimmersSimple nd)) ->
    (t -> m DS18B20) ->
    m (Mix ni nd no)
mix transport' dinputs' aoutputs' dimmers' ds18b20 = do
    let aoOffset = fromIntegral $ natVal (aNat :: NatType no)
    transport <- transport'
    dinputs <- dinputs' True transport
    aoutputs <- aoutputs' transport
    dimmers <- dimmers' aoOffset transport
    shouldInit <- asks D.shouldInit
    ds18b20 transport
    pure Mix{dinputs, dimmers, aoutputs, shouldInit}

instance (KnownNat ni, KnownNat nd, KnownNat no) => Controller (Mix ni nd no) where
    handle mix buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo mix buff size
            , action ==? actionDim ==> onDim mix buff size
            , action ==? actionInitialize ==> onInit mix buff size
            , action ==? actionGetState ==> onGetState mix
            ]

onInit :: forall l ni no nd s t. (KnownNat ni, KnownNat nd, KnownNat no, KnownNat l) 
       => Mix ni nd no 
       -> Buffer l Uint8 
       -> Uint8 
       -> Ivory (ProcEffects s t) ()
onInit Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType no)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    let sizeBuff = 1 + dimmersN + aoutputsN
    when (size >=? sizeBuff)  do
        offset <- local $ ival 1

        let aos = EAO.aoutputs $ AO.getAOutputs aoutputs
        arrayMap \ix -> do
            offset' <- deref offset
            let ao = aos ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (ao ~> EAO.value) (safeCast v / 255)
            store offset $ offset' + 1

        let dims = EDIM.dimmers $ DIM.getDimmers dimmers
        arrayMap \ix -> do
            offset' <- deref offset
            let dim = dims ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (dim ~> EDIM.value) (safeCast v / 255)
            store offset $ offset' + 1

        store shouldInit false

        pure()

onGetState :: (KnownNat ni, KnownNat nd, KnownNat no) 
           => Mix ni nd no 
           -> Ivory eff ()
onGetState Mix{..} = do
    forceSyncDInputs dinputs
    initialized <- iNot <$> deref shouldInit
    when initialized do
        AO.forceSync aoutputs
        DIM.forceSync dimmers 

onDo :: forall l ni no nd s t. (KnownNat l, KnownNat ni, KnownNat nd, KnownNat no) 
     => Mix ni nd no 
     -> Buffer l Uint8 
     -> Uint8 
     -> Ivory (ProcEffects s t) ()
onDo Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType no)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    when (size >=? 3) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? aoutputsN + dimmersN) do
                value' <- deref $ buff ! 2
                ifte_
                    (index <=? castDefault aoutputsN)
                    do
                        let index' = index - 1
                        ifte_
                            (value' ==? 0)
                            do AO.onOff (AO.getAOutputs aoutputs) index'
                            do AO.onOn  (AO.getAOutputs aoutputs) index'
                    do
                        let index' = index - 1
                        ifte_
                            (value' ==? 0)
                            do DIM.onOff (DIM.getDimmers dimmers) index'
                            do DIM.onOn  (DIM.getDimmers dimmers) index'


onDim :: forall l ni no nd s t. (KnownNat l, KnownNat ni, KnownNat nd, KnownNat no) 
      => Mix ni nd no 
      -> Buffer l Uint8 
      -> Uint8 
      -> Ivory (ProcEffects s t) ()
onDim Mix{..} buff size = do
    let aoutputsN = fromIntegral $ natVal (aNat :: NatType no)
    let dimmersN = fromIntegral $ natVal (aNat :: NatType nd)
    when (size >=? 3) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? aoutputsN + dimmersN) do
                ifte_
                    (index <=? castDefault aoutputsN)
                    do
                        let index' = index - 1
                        let ao = AO.getAOutputs aoutputs
                        action <- deref $ buff ! 2
                        cond_
                            [ action ==? 0 ==> AO.onOff ao index'
                            , action ==? 1 ==> AO.onOn  ao index'
                            , action ==? 2 ==> AO.onSet ao index' buff size
                            , action ==? 3 ==> AO.onSet ao index' buff size
                            ]
                    do
                        let index' = index - 1
                        let dim = DIM.getDimmers dimmers
                        action <- deref $ buff ! 2
                        cond_
                            [ action ==? 0 ==> DIM.onOff dim index'
                            , action ==? 1 ==> DIM.onOn  dim index'
                            , action ==? 2 ==> DIM.onSet dim index' buff size
                            , action ==? 3 ==> DIM.onSet dim index' buff size
                            ]
