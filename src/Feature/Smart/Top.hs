{-# LANGUAGE FlexibleContexts #-}
module Feature.Smart.Top where
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State  (MonadState)
import           Core.Context
import           Core.Controller      (Controller)
import qualified Core.Domain          as D
import           Core.Feature
import           Interface.GPIO.Input
import           Interface.UART



data Top = Top


mkTop :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i)
      => (p -> m u) -> (p -> m i) -> m Top
mkTop = undefined


top :: (MonadState Context m, MonadReader (D.Domain p t) m, UART u, Input i)
      => (p -> m u) -> (p -> m i) -> m Feature
top uart pin = do
    top <- mkTop uart pin
    pure $ Feature top



instance Controller Top
