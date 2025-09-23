module Core.Domain where

import Control.Monad.State
import Core.Context
import Core.Transport
import qualified Core.Version as V
import Data.Buffer
import Data.Record
import Data.Value
import qualified Interface.MCU as I
import qualified Interface.Mac as M
import Interface.SystemClock
import Ivory.Language
import Support.Cast
import Support.ReadAddr
import Support.Serialize
import Util.String

data Domain p i = Domain
       { model :: Value Uint8
       , version :: V.Version
       , mcu :: I.Platform p
       , mustInit :: IBool
       , shouldInit :: Value IBool
       , implementation :: i
       }

domain ::
       (MonadState Context m) =>
       Uint8 ->
       (Uint8, Uint8) ->
       I.Platform p ->
       IBool ->
       i ->
       m (Domain p i)
domain model' version' mcu mustInit implementation = do
       addModule inclCast
       addModule inclString
       addModule inclSerialize
       addModule inclReadAddr
       model <- value "model" model'
       version <- V.version "version" version'
       shouldInit <- value "should_init" mustInit
       pure Domain{model, version, mcu, mustInit, shouldInit, implementation}
