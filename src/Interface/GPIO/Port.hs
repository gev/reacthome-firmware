module Interface.GPIO.Port where

-- data Pull = PullUp
--           | PullDown
--           | PullNone

class Pull p d | p -> d where
  pullUp :: p -> d
  pullDown :: p -> d
  pullNone :: p -> d
