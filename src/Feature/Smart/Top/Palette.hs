module Feature.Smart.Top.Palette where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Meta (Meta (..))
import Data.Matrix
import Data.Value
import GHC.TypeNats
import Interface.Flash
import Interface.MCU (MCU (..))
import Ivory.Language

data Palette n l = Palette
    { palette :: Matrix n l Uint32
    , synced :: Values n IBool
    , etc :: Flash
    }

mkPalette ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , KnownNat n
    , KnownNat l
    ) =>
    m (Palette n l)
mkPalette = do
    meta <- asks D.meta
    palette <- matrix_ "palette"
    synced <- values' "palette_synced" false
    pure $
        Palette
            { palette
            , synced
            , etc = meta.mcu.etc
            }
