module Implementation.Smart.Top where

import           Control.Monad
import           Core.Controller
import           Feature.DInputs
import           Feature.Sht21


top :: Monad m => (Bool -> m DInputs) -> m SHT21 -> m ()
top dinputs sht21 = void $ dinputs True >> sht21
