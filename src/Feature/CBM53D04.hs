module Feature.CBM53D04 where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Task
import Core.Transport qualified as T
import Data.Buffer
import Data.Index
import Data.Serialize
import Data.Value hiding (value)
import Endpoint.AOutputs qualified as E
import GHC.TypeNats
import Interface.MCU
import Interface.SPI qualified as I
import Ivory.Language
import Ivory.Stdlib

type AONumber = 4

data CBM53D04
    = forall i.
      (I.SPI i) =>
    CBM53D04
    { n :: Uint8
    , spi :: i
    , order :: Buffer AONumber Uint8
    , getAOutputs :: E.AOutputs AONumber
    , shouldInit :: Value IBool
    , currentSync :: Index (Ix AONumber)
    , currentManage :: Index (Ix AONumber)
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

cbm53d04 ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport t
    , I.SPI i
    ) =>
    (p -> m i) ->
    [Uint8] ->
    t ->
    m CBM53D04
cbm53d04 spi' order' transport = do
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    spi <- spi' $ peripherals mcu
    order <- values "ao_orders" order'
    getAOutputs <- E.mkAOutputs "cbm53d04_aoutputs"
    currentSync <- index "current_analog_sync"
    currentManage <- index "current_analog_manage"
    let n = length order'

    let cbm53d04 =
            CBM53D04
                { n = fromIntegral n
                , spi
                , order
                , getAOutputs
                , shouldInit
                , currentSync
                , currentManage
                , transmit = T.transmitBuffer transport
                }

    addSync "analog_aoutputs" $ forceSync cbm53d04
    addTask $ yeld "analog_aoutputs_sync" $ sync cbm53d04
    addTask $ delay 1 "analog_aoutputs_manage" $ manage cbm53d04

    pure cbm53d04

manage :: CBM53D04 -> Ivory eff ()
manage cbm@CBM53D04{..} = do
    i <- deref currentManage
    let ao = E.aoutputs getAOutputs ! i
    v <- deref $ ao ~> E.value
    order' <- deref (order ! i)
    setVoltageReduced cbm order' v
    store currentManage $ i + 1

forceSync :: CBM53D04 -> Ivory eff ()
forceSync CBM53D04{..} =
    arrayMap \ix -> store (E.aoutputs getAOutputs ! ix ~> E.synced) false

sync :: CBM53D04 -> Ivory (ProcEffects s ()) ()
sync CBM53D04{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') do
        i <- deref currentSync
        let d = E.aoutputs getAOutputs ! i
        synced' <- deref $ d ~> E.synced
        when (iNot synced') do
            msg <- E.message getAOutputs (castDefault $ fromIx i)
            transmit msg
            store (d ~> E.synced) true
        store currentSync $ i + 1

onInit ::
    (KnownNat l) =>
    CBM53D04 ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit CBM53D04{..} buff size =
    when (size >=? 1 + n) do
        offset <- local $ ival 1
        let aos = E.aoutputs getAOutputs
        arrayMap \ix -> do
            offset' <- deref offset
            let ao = aos ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (ao ~> E.value) (safeCast v / 255)
            store offset $ offset' + 1
        store shouldInit false

onAo ::
    (KnownNat l) =>
    CBM53D04 ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onAo CBM53D04{..} buff size = do
    when (size >=? 3) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) do
                let index' = index - 1
                onSet getAOutputs index' buff size


onSet ::
    (KnownNat n, KnownNat l) =>
    E.AOutputs n ->
    Uint8 ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onSet aoutputs' index buff size =
    when (size >=? 3) do
        value <- unpack buff 2 :: Ivory eff Uint8
        E.set aoutputs' index (safeCast value / 255)

setVoltageReduced :: CBM53D04 -> Uint8 -> IFloat -> Ivory eff ()
setVoltageReduced CBM53D04{..} channel value = do
    let resolution = 8

    let offsetChannel = 14
    let offsetPd = 13
    let offsetLdac = 12
    let offsetValue = 4

    let regChannel = safeCast channel `iShiftL` offsetChannel
    let regPd = 1 `iShiftL` offsetPd
    let regLdac = 0 `iShiftL` offsetLdac

    let value' = value * ((2 ** resolution) - 1)
    let regValue = castDefault value' `iShiftL` offsetValue

    let register = regChannel .| regPd .| regLdac .| regValue

    I.transmit spi register
