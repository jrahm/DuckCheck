module Main where

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Data.Set (Set, fromList)

import DuckTest.Flags
import DuckTest

flags :: [OptDescr Flag]
flags = [  Option ['2'] [] (NoArg Version2)
            "The source file is a Python 2 program, not a Python 3 program."
         , Option ['v'] [] (NoArg Verbose)
            "Run in verbose logging mode." ]

runFilesWithArgs :: [Flag] -> [String] -> IO ()
runFilesWithArgs opts =
    let optset = fromList opts in
        mapM_ (runDuckTestOnOneFile optset)

main :: IO ()
main = (>>=) getArgs $ \argv ->
           case getOpt Permute flags argv of

            (args, fs, []) -> do
                let files = if null fs then ["-"] else fs
                runFilesWithArgs args fs

            (_, _, errs) -> do
                hPutStrLn stderr (concat errs ++ usageInfo header flags)
                exitWith (ExitFailure 1)

        where header = "Usage: hiss [args] [file ...]"
