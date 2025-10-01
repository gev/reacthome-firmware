module Interface.Display where

import Data.Value (Values)
import Ivory.Language

data Render n d = Render
    { display :: d
    , frameRate :: Uint32
    , frame :: Values n Uint8
    , render :: forall s. Ivory (ProcEffects s ()) IBool
    }

class Display d
