module Interface.NeoPixel where
    
import           Ivory.Language
import           Interface.PWM

data RGB = RGB 
    { g :: Record Uint8
    , r :: Record Uint8
    , b :: Record Uint8
    }   

class NeoPixel n where
    sendColors :: n -> IvoryEff ()
    setPixelColor :: n -> IvoryEff ()