module Interface.MCU where

import Control.Monad.State
import Core.Context
import Data.Buffer
import Interface.Mac
import Interface.SystemClock (SystemClock)
import Ivory.Language

data Platform p = Platform
    { systemClock :: SystemClock
    , peripherals :: p
    , mac :: Mac
    }

data MCU p = MCU
    { platform :: forall m. (MonadState Context m) => m (Platform p)
    , model :: String
    , modification :: String
    }

mkPlatform ::
    (MonadState Context m) =>
    m SystemClock ->
    (Buffer 6 Uint8 -> forall s. Ivory (ProcEffects s ()) ()) ->
    ModuleDef ->
    p ->
    m (Platform p)
mkPlatform systemClock' initializeMac mcuModule peripherals = do
    addModule mcuModule
    systemClock <- systemClock'
    mac <- makeMac initializeMac "mac"
    pure Platform{systemClock, peripherals, mac}
