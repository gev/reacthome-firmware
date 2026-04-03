module Core.Domain where

import Control.Monad.State
import Core.Context
import Core.Version qualified as V
import Data.Value
import Interface.MCU qualified as I
import Ivory.Language
import Support.Cast
import Support.ReadAddr
import Support.RunAppByAddr
import Support.Serialize
import Util.String

data Domain p i = Domain
    { model :: Value Uint8
    , version :: V.Version
    , mcu :: I.Platform p
    , board :: Uint8
    , mustInit :: IBool
    , shouldInit :: Value IBool
    , implementation :: i
    }

domain ::
    (MonadState Context m) =>
    Uint8 ->
    (Uint8, Uint8) ->
    I.Platform p ->
    Uint8 ->
    IBool ->
    i ->
    m (Domain p i)
domain model' version' mcu board mustInit implementation = do
    addModule inclCast
    addModule inclString
    addModule inclSerialize
    addModule inclReadAddr
    addModule inclRunAppByAddr
    model <- value "model" model'
    version <- V.version "version" version'
    shouldInit <- value "should_init" mustInit
    pure Domain{model, version, mcu, board, mustInit, shouldInit, implementation}
