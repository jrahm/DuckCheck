{-# LANGUAGE TupleSections #-}
module DuckTest.Infer.AST where

{- This module will, for each statement in an AST, infer
 - the type of that statement, as well as collect the
 - variables needed. -}

import DuckTest.Types
import DuckTest.Internal.Common

import DuckTest.Monad
import DuckTest.Infer.Functions
import DuckTest.Infer.Classes

import qualified Data.Map as Map

data BoundVariable = BoundVariable {
      variable_name :: String
    , variable_type :: PyType
}

data Annotation = Annotation {
      annotation_bound :: Maybe BoundVariable -- a variable bound in the statement
    , annotation_unbound :: Map String PyType -- a map of needed variables to their types
}

inferStatements :: [Statement a] -> DuckTest a [(Annotation, Statement a)]
inferStatements = mapM (\s -> (,s) <$> f s)
    where
        f st@(Fun (Ident name _) _ _ _ _) = do
            {- A function really binds a value to an element -}
            typ <- inferTypeForFunction st
            return $ Annotation (Just $ BoundVariable name $ Functional typ) Map.empty

        f cls@(Class {class_name = (Ident name _)}) = do
            {- A class really creates a function with that class name -}
            typ <- inferTypeForClass cls
            return $ Annotation (Just $ BoundVariable name $ Functional typ) Map.empty
