module Implementation.AO4 where

import Control.Monad.Reader
import Control.Monad.State
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Serialize
import Endpoint.AOutputs qualified as A
import Feature.CBM53D04
import Feature.GetInfo
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Support.Cast

type SizeSyncStateBuff n = 1 + n

data AO4 = AO4
    { aoutput4 :: CBM53D04
    , syncStateBuff :: Buffer (SizeSyncStateBuff AONumber) Uint8
    , info :: GetInfo
    }

ao4 ::
    ( Monad m
    , KnownNat (SizeSyncStateBuff AONumber)
    , MonadState Context m
    , LazyTransport t
    , MonadReader (D.Domain p i) m
    ) =>
    (t -> m CBM53D04) -> m t -> m AO4
ao4 sbm53d04' transport' = do
    transport <- transport'
    aoutput4 <- sbm53d04' transport
    syncStateBuff <- buffer "sync_channels"

    info <- mkGetInfo transport

    let ao4 = AO4{aoutput4, syncStateBuff, info}

    addTask $ delay 5_000 "sync_channels" $ syncChannels ao4

    pure ao4

instance Controller AO4 where
    handle AO4{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionAo ==> onAo aoutput4 buff size
            , action ==? actionInitialize ==> onInit aoutput4 buff size
            , action ==? actionGetState ==> forceSync aoutput4
            , action ==? actionGetInfo ==> onGetInfo info
            ]

syncChannels ::
    forall s t.
    (KnownNat (SizeSyncStateBuff AONumber)) =>
    AO4 ->
    Ivory (ProcEffects s t) ()
syncChannels AO4{..} = do
    shouldInit' <- deref $ shouldInit aoutput4
    when (iNot shouldInit') do
        arrayMap \ix -> store (syncStateBuff ! ix) 0
        pack syncStateBuff 0 actionGetState
        let offset = 1
        arrayMap \ix -> do
            let aoutput = A.aoutputs (getAOutputs aoutput4) ! ix
            value <- castFloatToUint8 . (* 255) =<< deref (aoutput ~> A.value)
            let ixBuff = toIx . (+ offset) $ fromIx ix
            pack syncStateBuff ixBuff value
        transmit aoutput4 syncStateBuff