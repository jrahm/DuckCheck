{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DuckTest.Internal.Format where

import DuckTest.Internal.Common
import qualified Data.Map as Map

import DuckTest.Monad
import DuckTest.Types



class DuckShowable a where
    duckShow :: LogLevel -> a -> DuckTest e String

instance DuckShowable (LogLevel -> String) where
    duckShow ll f = return $ f ll

class DIsChar ch where
    charToString :: [ch] -> String
    fromStringToChar :: String -> [ch]

instance DIsChar Char where
    charToString = id
    fromStringToChar = id

instance (DIsChar a) => DuckShowable [a] where
    duckShow _ c = return $ charToString c

instance (DuckShowable a, DuckShowable b) => DuckShowable (a, b) where
    duckShow ll (a, b) = do
        v1 <- duckShow ll a
        v2 <- duckShow ll b
        return $ v1 ++ v2

class LogResult r where
    logr :: (DuckShowable a) => a -> r

instance (DuckShowable a, LogResult r) => LogResult (a -> r) where
    logr fst' snd' = logr (fst', snd')

instance (DIsChar ch) => LogResult (DuckTest e [ch]) where
    logr a = do
        ll <- getLogLevel
        str <- duckShow ll a
        return $ fromStringToChar str

instance (DuckShowable (Expr a)) where
    duckShow _ = return . prettyText

instance (DuckShowable (Statement a)) where
    duckShow _ = return . prettyText

duckf :: (DuckShowable a, LogResult r) => a -> r
duckf = logr

warnTypeError :: e -> TypeError -> DuckTest e ()
warnTypeError pos (Incompatible t1 t2) =
    warn pos $ duckf "Incompatible types " Green t1 Reset " and " Green t2 Reset
warnTypeError pos (Difference t1 t2 dif) =
    warn pos $ do
        attrsNeeded <-
                forM (Map.toList dif) $ \(key, typ) ->
                    duckf key " :: " typ :: DuckTest e String
        duckf "'" Green t2 Reset "' incompatible as '" Green t1 Reset "'. Missing attributes needed: '" Green (intercalate ", " attrsNeeded) Reset "'"

data Ansi = Green | Red | Yellow | Blue | Reset | Bold

instance DuckShowable Ansi where
    duckShow _ color = do
        isterm <- runningInTerminal
        if not isterm then return "" else return $
            case color of
                Red -> "\x1b[31m"
                Green -> "\x1b[32m"
                Yellow -> "\x1b[33m"
                Blue -> "\x1b[34m"
                Reset -> "\x1b[00m"
                Bold -> "\x1b[01m"

-- warnTypeError :: e -> TypeError -> DuckTest e ()
-- warnTypeError pos (Incompatible t1 t2) =
--     warn (printf "Incompatible types %s and %s" (show t1) (show t2)) pos
--     warn pos $ duckf t1 t2

instance (DuckShowable PyType) where
    duckShow logl t = return $ duckShow' logl t
      where
        duckShow' ll (Scalar (Just name) _) | ll > Trace = name
        duckShow' ll (Scalar Nothing s) | ll > Trace = "{ " ++ intercalate ", " (Map.keys s) ++ " }"
        duckShow' ll (Functional args ret) | ll > Trace = "(" ++ intercalate ", " (map (\(a, b) -> a ++ " :: " ++ duckShow' ll b) args) ++ ") -> " ++ duckShow' ll ret
        duckShow' ll (Alpha _) | ll > Trace = "alpha"
        duckShow' Trace typ = prettyType' True typ
        duckShow' _ typ = prettyType' False typ
