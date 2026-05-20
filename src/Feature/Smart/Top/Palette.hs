module Feature.Smart.Top.Palette where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Data.Matrix
import Data.Value
import GHC.TypeNats
import Interface.Flash
import Ivory.Language
import Core.Meta

data Palette n l = forall p. Palette
    { palette :: Matrix n l Uint32
    , synced :: Values n IBool
    , etc :: Flash p
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
            , etc = mkEtc meta
            }
