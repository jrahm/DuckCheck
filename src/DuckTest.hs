{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module DuckTest where

import DuckTest.Internal.Common

import System.IO
import System.Posix.Terminal
import System.Posix.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

import DuckTest.MonadHelper
-- import DuckTest.Insanity
import DuckTest.Checker
import DuckTest.Flags
import DuckTest.Monad
import DuckTest.AST.Preprocess
import DuckTest.Builtins
import DuckTest.Parse
import DuckTest.Internal.State.Instance

runDuckTestM :: FilePath -> DuckTest SrcSpan ()
runDuckTestM fp =
    (>>=) (parsePython fp) $
            whenJust' $ \(Module stmts', _) -> do
                let stmts = preprocess stmts'
                Trace %% intercalate "\n" (map prettyText stmts)
                unless' (hasFlag PreprocessOnly) $
                    runChecker_ initState stmts

getStartPos :: SrcSpan -> Maybe (String, Int, Int)
getStartPos sp = case sp of
    (SpanCoLinear fn r c _) -> Just (fn, r, c)
    (SpanMultiLine fn r c _ _) -> Just (fn, r, c)
    (SpanPoint fn r c) -> Just (fn, r, c)
    _ -> Nothing

runDuckTestOnOneFile :: Set Flag -> LogLevel -> FilePath -> IO ()
runDuckTestOnOneFile flags ll file = do
    st <- runDuckTestIO flags ll (runDuckTestM file)
    isTerm <- queryTerminal (Fd 1)

    let (styleBegin, styleEnd) =
            if isTerm then ("\x1b[01;31m", "\x1b[0m") else ("", "")

    forM_ (sort $ getWarnings st) $ \(err, pos) ->
        whenJust (getStartPos pos) $ \(f, r, c) ->
            hPutStr stderr $ printf "%s%s(%d:%d):%s %s\n" styleBegin f r c styleEnd err
