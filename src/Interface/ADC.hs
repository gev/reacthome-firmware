module Interface.ADC where 

import           Ivory.Language

class ADC a where
    getAnalog  :: a -> Ivory eff Uint16