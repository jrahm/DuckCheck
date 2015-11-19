{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module DuckTest where

import Control.Monad (foldM, when, unless, void, forM, zipWithM_)
import Data.Foldable (mapM_, forM_)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import DuckTest.AST.Util
import DuckTest.Flags
import DuckTest.Monad
import DuckTest.Infer.Functions
import Language.Python.Common
import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3
import Prelude hiding (mapM_)
import System.IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Posix.Terminal
import System.Posix.Types

import DuckTest.Builtins
import DuckTest.Types
import DuckTest.MonadHelper
import DuckTest.Insanity
import DuckTest.AST.BinaryOperators
import DuckTest.Checker

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
            whenJust' $ \(Module stmts, _) ->
                runChecker detectInsanity Map.empty stmts

getStartPos :: SrcSpan -> Maybe (String, Int, Int)
getStartPos sp = case sp of
    (SpanCoLinear fn r c _) -> Just (fn, r, c)
    (SpanMultiLine fn r c _ _) -> Just (fn, r, c)
    (SpanPoint fn r c) -> Just (fn, r, c)
    _ -> Nothing

runDuckTestOnOneFile :: Set Flag -> FilePath -> IO ()
runDuckTestOnOneFile flags file = do
    st <- runDuckTestIO flags (runDuckTestM file)
    isTerm <- queryTerminal (Fd 1)

    let (styleBegin, styleEnd) =
            if isTerm then ("\x1b[01;31m", "\x1b[0m") else ("", "")

    forM_ (getWarnings st) $ \(err, pos) ->
        whenJust (getStartPos pos) $ \(f, r, c) ->
            hPutStr stderr $ printf "%s%s(%d:%d):%s %s\n" styleBegin f r c styleEnd err
