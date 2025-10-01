module Implementation.DI where

import Core.Actions
import Core.Controller
import Feature.DInputs (DInputs, forceSyncDInputs)
import Feature.DS18B20
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

newtype DI n = DI {dinputs :: DInputs n}

di ::
    (Monad m) =>
    m t ->
    (Bool -> t -> m (DInputs n)) ->
    (t -> m DS18B20) ->
    m (DI n)
di transport' dinputs' ds18b20 = do
    transport <- transport'
    ds18b20 transport
    dinputs <- dinputs' True transport
    pure DI{dinputs}

instance (KnownNat n) => Controller (DI n) where
    handle DI{..} buff _ = do
        action <- deref $ buff ! 0
        cond_
            [ action
                ==? actionGetState
                ==> forceSyncDInputs dinputs
            ]
