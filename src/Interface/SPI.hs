module Interface.SPI where

import Ivory.Language

class SPI i where
    transmit :: i -> Uint16 -> Ivory eff ()