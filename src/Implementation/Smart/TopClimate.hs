module Implementation.Smart.TopClimate where

import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Core.Actions (actionGetInfo)
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Transport
import Feature.GetInfo
import Feature.Sht21 (SHT21)
import Ivory.Language
import Ivory.Stdlib

data Top = Top
    { sht21 :: SHT21
    , info :: GetInfo
    }

topClimate ::
    ( MonadState Context m
    , LazyTransport t
    , MonadReader (D.Domain p i) m
    , LazyTransport t
    ) =>
    (t -> m SHT21) ->
    m t ->
    m Top
topClimate sht21' transport' = do
    transport <- transport'
    sht21 <- sht21' transport
    info <- mkGetInfo transport

    pure Top{sht21, info}

instance Controller Top
handle Top{..} buff _ = do
    action <- deref $ buff ! 0
    cond_
        [ action ==? actionGetInfo ==> onGetInfo info
        ]