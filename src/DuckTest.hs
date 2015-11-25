{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module DuckTest where

import DuckTest.Internal.Common

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

import System.IO
import System.Posix.Terminal
import System.Posix.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

import DuckTest.MonadHelper
import DuckTest.Insanity
import DuckTest.Checker
import DuckTest.Flags
import DuckTest.Monad
import DuckTest.AST.Preprocess

parsePython :: FilePath -> DuckTest SrcSpan (Maybe (ModuleSpan, [Token]))
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    case (if version2 then P2.parseModule else P3.parseModule) sourceCode fp of
        Left (UnexpectedToken token) -> emitWarning "ParseError: Unexpected token" (token_span token) >> return Nothing
        Left (UnexpectedChar ch NoLocation) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) SpanEmpty >> return Nothing
        Left (UnexpectedChar ch (Sloc f l c)) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) (SpanPoint f l c) >> return Nothing
        Left (StrError str) -> emitWarning ("ParseError: %s" ++ str) SpanEmpty >> return Nothing
        Right a -> return (Just a)


runDuckTestM :: FilePath -> DuckTest SrcSpan ()
runDuckTestM fp =
    (>>=) (parsePython fp) $
            whenJust' $ \(Module stmts', _) -> do
                let stmts = preprocess stmts'
                Trace %% intercalate "\n" (map prettyText stmts)
                runChecker detectInsanity Map.empty stmts

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

    forM_ (getWarnings st) $ \(err, pos) ->
        whenJust (getStartPos pos) $ \(f, r, c) ->
            hPutStr stderr $ printf "%s%s(%d:%d):%s %s\n" styleBegin f r c styleEnd err
