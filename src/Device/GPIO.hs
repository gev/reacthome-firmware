module Device.GPIO where

newtype PA a = PA a
newtype PB a = PB a

data I2C a = I2C
  { scl :: a
  , sda :: a
  }

data SPI a = SPI
  { sckl :: a
  , miso :: a
  , mosi :: a
  , ss   :: a
  }

data USART a = USART
  { rx :: a
  , tx :: a
  }
