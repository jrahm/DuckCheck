module Hiss where

import Data.Set (Set)
import Hiss.Monad
import Hiss.Flags

runHissM :: FilePath -> Hiss ()
runHissM = undefined

runHissOnOneFile :: Set Flag -> FilePath -> IO ()
runHissOnOneFile flags file = runHiss flags (runHissM file)
