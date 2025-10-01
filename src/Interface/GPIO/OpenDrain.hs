module Interface.GPIO.OpenDrain (
    Input,
    Output,
    OpenDrain,
    get,
    reset,
    set,
) where

import Interface.GPIO.Output

class (Output a) => OpenDrain a
