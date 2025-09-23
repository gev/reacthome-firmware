module Implementation.Dimmer where

import Core.Actions
import Core.Controller
import Feature.Dimmers
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib

newtype Dimmer n = Dimmer
    {dimmers :: Dimmers n}

dimmer :: (Monad m) => m t -> (t -> m (Dimmers n)) -> m (Dimmer n)
dimmer transport' dimmers' = do
    transport <- transport'
    dimmers <- dimmers' transport
    pure Dimmer{dimmers}

instance (KnownNat n) => Controller (Dimmer n) where
    handle Dimmer{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo dimmers buff size
            , action ==? actionDim ==> onDim dimmers buff size
            , action ==? actionInitialize ==> onInit dimmers buff size
            , action ==? actionGetState ==> onGetState dimmers
            ]
