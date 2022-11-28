module Device.GPIO where

data GPIO  a
  = IN     a
  | OUT    a
  | I2C
    { scl :: a
    , sda :: a
    }
  | SPI
    { sckl :: a
    , miso :: a
    , mosi :: a
    , ss   :: a
    }
  | UART
    { rx :: a
    , tx :: a
    }
