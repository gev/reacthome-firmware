module Main where
import           Blink (compileBlink)
import           Shake (shake)

main :: IO ()
main = do
  compileBlink
  shake
