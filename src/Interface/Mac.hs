module Interface.Mac where

import Control.Monad.State
import Core.Context
import Data.Value
import Ivory.Language

type Mac = Values 6 Uint8

makeMac ::
    (MonadState Context m) =>
    (Values 6 Uint8 -> forall s. Ivory (ProcEffects s ()) ()) ->
    String ->
    m Mac
makeMac initMac name = do
    mac <- values_ name
    addInit name $ initMac mac
    pure mac
