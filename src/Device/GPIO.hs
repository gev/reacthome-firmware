module Device.GPIO where

data PIO a
  = PA a | PB a

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

data GPIO a = GPIO
  { pio   :: [PIO   a]
  , i2c   :: [I2C   a]
  , spi   :: [SPI   a]
  , usart :: [USART a]
  }

gpio :: GPIO a
gpio = GPIO [] [] [] []
