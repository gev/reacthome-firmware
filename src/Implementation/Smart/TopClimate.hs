module Implementation.Smart.TopClimate where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Controller
import Core.Transport
import Feature.Sht21 (SHT21)

newtype Top = Top {sht21 :: SHT21}

topClimate ::
    ( MonadState Context m
    , LazyTransport t
    ) =>
    m t ->
    (t -> m SHT21) ->
    m Top
topClimate transport' sht21' = do
    transport <- transport'
    sht21 <- sht21' transport

    pure Top{sht21}

instance Controller Top