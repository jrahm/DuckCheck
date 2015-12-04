module DuckTest.Parse where

import DuckTest.Internal.Common
import DuckTest.Monad

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

parsePython :: FilePath -> DuckTest SrcSpan (Maybe (ModuleSpan, [Token]))
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- dtReadFile fp
    case (if version2 then P2.parseModule else P3.parseModule) sourceCode fp of
        Left (UnexpectedToken token) -> emitWarning "ParseError: Unexpected token" (token_span token) >> return Nothing
        Left (UnexpectedChar ch NoLocation) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) SpanEmpty >> return Nothing
        Left (UnexpectedChar ch (Sloc f l c)) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) (SpanPoint f l c) >> return Nothing
        Left (StrError str) -> emitWarning ("ParseError: %s" ++ str) SpanEmpty >> return Nothing
        Right a -> return (Just a)
