module Hiss where

import Data.Set (Set)
import Hiss.Monad
import Hiss.Flags

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

import Language.Python.Common

parsePython :: FilePath -> Hiss (ModuleSpan, [Token])
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    fromEither $
        (if version2 then P2.parseModule else P3.parseModule) fp sourceCode


runHissM :: FilePath -> Hiss ()
runHissM fp = do
    ast <- parsePython fp
    hlog (show ast)

runHissOnOneFile :: Set Flag -> FilePath -> IO ()
runHissOnOneFile flags file = runHissIO flags (runHissM file)
