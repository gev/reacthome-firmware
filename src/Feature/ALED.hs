{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

module Feature.ALED where

import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain          as D
import           Core.Handler
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.ALED        as E
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Display    (Display, Render (Render))
import           Interface.Mac
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast



data ALED ng ns np = forall d. Display d => ALED
    { display :: d
    , getALED :: E.ALED ng ns np
    }


maxValue = 0.3 :: IFloat

aled :: ( MonadState Context m
        , MonadReader (D.Domain p c) m
        , Display d, Handler (Render np) d
        , KnownNat ng, KnownNat ns, KnownNat np
        , T.Transport t
        ) => (p -> m d) -> t -> m (ALED ng ns np)
aled mkDisplay transport = do
    mcu         <- asks D.mcu
    display     <- mkDisplay $ peripherals mcu
    getALED     <- E.mkALED

    let aled = ALED { display, getALED }

    addTask $ yeld "aled" $ update aled

    addHandler $ Render display 24 (E.subPixels getALED) $ do
        update aled
        render aled

    pure aled



update :: KnownNat np => ALED ng ns np -> Ivory (ProcEffects s ()) ()
update ALED{..} = do
    let p = E.subPixels getALED
    v <- deref $ p ! 0
    store (p ! 0) $ v + 1


render :: ALED ng ns np -> Ivory (ProcEffects s ()) ()
render ALED{..} =
    pure ()
