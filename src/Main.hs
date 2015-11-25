module Main where

import DuckTest.Internal.Common

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
         , Option ['v'] [] (OptArg verbosity "0-4")
            "Run in verbose logging mode."
         , Option ['P'] [] (NoArg PreprocessOnly)
            "Stop after preprocessing"]
     where
        verbosity Nothing = Verbose Info
        verbosity (Just s) = case s of
                    "0" -> Verbose Error
                    "1" -> Verbose Warn
                    "2" -> Verbose Info
                    "3" -> Verbose Debug
                    "4" -> Verbose Trace
                    _ -> Verbose Trace

runFilesWithArgs :: [Flag] -> [String] -> IO ()
runFilesWithArgs opts =
    let (Verbose ll) = fromMaybe (Verbose Warn) $ listToMaybe (filter isVerboseFlag opts)
        optset = fromList opts in
            mapM_ (runDuckTestOnOneFile optset ll)

    where isVerboseFlag (Verbose _) = True
          isVerboseFlag _ = False

main :: IO ()
main = (>>=) getArgs $ \argv ->
           case getOpt Permute flags argv of

            (args, fs, []) -> do
                let files = if null fs then ["-"] else fs
                runFilesWithArgs args fs

            (_, _, errs) -> do
                hPutStrLn stderr (concat errs ++ usageInfo header flags)
                exitWith (ExitFailure 1)

        where header = "Usage: ducktest [args] [file ...]"
