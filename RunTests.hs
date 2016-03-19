{-# LANGUAGE TupleSections #-}
module Main where

import System.Directory
import System.Process
import System.IO
import Data.List
import Safe
import Data.Set (member, fromList, Set, delete)
import Data.Maybe
import Control.Monad
import qualified Data.Set as S

green :: String -> String
green s = "\ESC[01;32m" ++ s ++ "\ESC[00m"

red :: String -> String
red s = "\ESC[01;31m" ++ s ++ "\ESC[00m"

blue :: String -> String
blue s = "\ESC[01;34m" ++ s ++ "\ESC[00m"

testFile :: FilePath -> IO Int
testFile fp = do
    putStrLn $ blue "Test: " ++ fp

    fileLines <- (zip [(1::Int)..] . lines) <$> readFile fp
    let errorLines = (filter isErrorLine fileLines)
        errorSet = fromList $ map fst errorLines
    errors <- (mapMaybe toErrorLine) <$> runDuckTest

    (passed, set) <- foldM checkError (True, errorSet) errors
    if not (S.null set) then do
        forM_ (S.toList set) (\i -> putStrLn $ "Expected error on line " ++ show i)
        putStrLn $ red "[Failed]" ++ " - " ++ fp
        return 0
     else
        if passed then do
            putStrLn $ green "[Passed]" ++ " - " ++ fp
            return 1
         else do
            putStrLn $ red "[Failed]" ++ " - " ++ fp
            return 0

    where
        checkError :: (Bool, Set Int) -> (Int, String) -> IO (Bool, Set Int)
        checkError (pass, errorSet) (err, linestr) =
            if not (err `member` errorSet) then do
                putStrLn $ red "Superfluous Error: " ++ linestr
                return (False, S.delete err errorSet)
             else do
                return (pass, S.delete err errorSet)

        isErrorLine (_, line) = "Error" `isSuffixOf` line

        toErrorLine :: String -> Maybe (Int, String)
        toErrorLine s =
            let s' = dropWhile (/='(') s
                (_:linenr) = takeWhile (/=':') s'
                in
                if null s' then Nothing else
                    (,s) <$> readMay linenr

        runDuckTest = do
            (_, _, err, _) <- runInteractiveCommand ("./ducktest " ++ fp)
            lines <$> hGetContents err

main :: IO()
main = do
    passed <- (mapM testFile =<< ((filterM isPython) =<< map ("tests/"++) <$> getDirectoryContents "tests/")) :: IO [Int]
    let x :: Int
        x = length passed
    let s :: Int
        s = sum passed

    putStrLn $ blue $ "Passed " ++ show s ++ "/" ++ show x

    where
        isPython s = liftM2 (&&) (doesFileExist s) (return $ ".py" `isSuffixOf` s)

