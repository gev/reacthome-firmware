module Main where
import           Blink     (compileBlink)
import           Scheduler (compileScheduler)
import           Shake     (shake)
import           USART     (compileUSART)

main :: IO ()
main = do
  compileBlink
  compileUSART
  compileScheduler
  shake
