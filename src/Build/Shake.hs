module Build.Shake  where

import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util


class Shake c where
    shake :: c -> [String] -> IO ()
