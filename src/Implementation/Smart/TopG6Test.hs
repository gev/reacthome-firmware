module Implementation.Smart.TopG6Test where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Controller
import Core.Transport

import Feature.Touches qualified as FT
import GHC.TypeNats

newtype Top n = Top
    { touches :: FT.Touches n
    }

topG6Test ::
    ( MonadState Context m
    , LazyTransport t
    , KnownNat n
    ) =>
    m t ->
    (t -> m (FT.Touches n)) ->
    m (Top n)
topG6Test transport' touches' = do
    transport <- transport'
    touches <- touches' transport
    let top = Top{touches}

    pure top

instance (KnownNat n) => Controller (Top n) where
    handle _ _ _ = pure ()
