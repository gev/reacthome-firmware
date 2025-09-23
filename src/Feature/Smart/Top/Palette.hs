{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Feature.Smart.Top.Palette where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Core.Context
import qualified Core.Domain as D
import Data.Color
import Data.Matrix
import Data.Value
import GHC.TypeNats
import Interface.Flash
import Ivory.Language

data Palette n l = forall f. (Flash f) => Palette
    { palette :: Matrix n l Uint32
    , synced :: Values n IBool
    , etc :: f
    }

mkPalette ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Flash f
    , KnownNat n
    , KnownNat l
    ) =>
    f ->
    m (Palette n l)
mkPalette etc = do
    palette <- matrix_ "palette"
    synced <- values' "palette_synced" false
    pure $ Palette{palette, synced, etc}
