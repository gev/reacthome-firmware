module Main where
import           Blink (compileBlink)
import           Shake (shake)
import           USART (compileUSART)

main :: IO ()
main = do
  compileBlink
  compileUSART
  shake
