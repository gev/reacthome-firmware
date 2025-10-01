module Interface.Display where

import Control.Monad.State
import Core.Context
import Core.Handler
import Data.Buffer
import Data.Value (Values)
import GHC.TypeNats
import Ivory.Language

data Render n d = Render
    { display :: d
    , frameRate :: Uint32
    , frame :: Values n Uint8
    , render :: forall s. Ivory (ProcEffects s ()) IBool
    }

class Display d
