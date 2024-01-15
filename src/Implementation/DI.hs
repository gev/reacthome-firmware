module Implementation.DI where

import           Control.Monad
import           Feature.DInputs
import           Feature.DS18B20


di :: Monad m => (Bool -> m DInputs) -> m DS18B20 -> m ()
di dinputs ds18b20 = void $ dinputs True >> ds18b20
