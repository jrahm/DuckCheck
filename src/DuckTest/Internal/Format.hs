{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DuckTest.Internal.Format where

import DuckTest.Internal.Common
import qualified Data.Map as Map

import DuckTest.Monad
import DuckTest.Types

import Debug.Trace

class DuckShowable a where
    duckShow :: LogLevel -> a -> String

instance DuckShowable (LogLevel -> String) where
    duckShow ll f = f ll

class DIsChar ch where
    charToString :: [ch] -> String
    fromStringToChar :: String -> [ch]

instance DIsChar Char where
    charToString = id
    fromStringToChar = id

instance (DIsChar a) => DuckShowable [a] where
    duckShow = const charToString

instance (DuckShowable a, DuckShowable b) => DuckShowable (a, b) where
    duckShow ll (a, b) = duckShow ll a ++ duckShow ll b

class LogResult r where
    logr :: (DuckShowable a) => a -> r

instance (DuckShowable a, LogResult r) => LogResult (a -> r) where
    logr fst snd = logr (fst, snd)

instance (DIsChar ch) => LogResult (DuckTest e [ch]) where
    logr a =
        fromStringToChar <$> (duckShow <$> getLogLevel <*> pure a)

instance (DuckShowable (Expr a)) where
    duckShow _ = prettyText

duckf :: (DuckShowable a, LogResult r) => a -> r
duckf = logr

warnTypeError :: e -> TypeError -> DuckTest e ()
warnTypeError pos (Incompatible t1 t2) =
    warn pos $ duckf "Incompatible types " t1 " and " t2
warnTypeError pos (Difference t1 t2 dif) =
    warn pos $ duckf t1 " incompatible as " t2 ". Type " t1 " missing attributes needed: " (intercalate ", " (map (intercalate ".") dif))


-- warnTypeError :: e -> TypeError -> DuckTest e ()
-- warnTypeError pos (Incompatible t1 t2) =
--     warn (printf "Incompatible types %s and %s" (show t1) (show t2)) pos
--     warn pos $ duckf t1 t2

instance (DuckShowable PyType) where
    duckShow ll (Scalar (Attributes (Just name) s)) | ll > Trace = name
    duckShow ll (Scalar (Attributes Nothing s)) | ll > Trace = "{ " ++ (intercalate ", " $ Map.keys s) ++ " }"
    duckShow ll (Functional args ret) | ll > Trace = "(" ++ intercalate ", " (map (\(a, b) -> duckShow ll a ++ " :: " ++ duckShow ll b) args) ++ ") -> " ++ duckShow ll ret
    duckShow ll (Alpha n _) | ll > Trace = n
    duckShow Trace t = prettyType' True t
    duckShow _ t = prettyType' False t
