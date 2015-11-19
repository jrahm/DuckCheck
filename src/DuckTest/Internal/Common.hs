module DuckTest.Internal.Common
    (module X, module DuckTest.Internal.Common) where

import Data.Set as X (Set)
import Data.Map as X (Map)

import Language.Python.Common as X hiding (empty)

import Control.Monad as X
import Control.Applicative as X

import Data.List as X
import Data.Maybe as X

import Text.Printf as X

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap fn = mconcat . map fn

mconcatMapM :: (Monoid mo, Monad m) => (a -> m mo) -> [a] -> m mo
mconcatMapM fn lst = mconcat <$> mapM fn lst

mconcatMapMaybe :: (Monoid m) => (a -> Maybe m) -> [a] -> m
mconcatMapMaybe fn = mconcat . mapMaybe fn
