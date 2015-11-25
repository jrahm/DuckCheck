{- This module is used to preprocess the AST before we
 - actually use it -}

module DuckTest.AST.Preprocess (preprocess) where

import DuckTest.Internal.Common
import DuckTest.AST.Util
import DuckTest.AST.BinaryOperators

import Debug.Trace

import qualified Data.Map as Map

dunderFunctions :: Map String String
dunderFunctions = Map.fromList [
        ("len", "__len__"), ("str", "__str__"),
        ("int", "__int__")
    ]

preprocess :: [Statement e] -> [Statement e]
preprocess =
    map (mapExpressions proc)
    where
        proc (BinaryOp op ex1 ex2 pos) =
            {- Convert all binary operators to their actual function calls
             - to make things easier -}
            mapExpressions proc $
                Call (Dot ex1 (Ident (toDunderName op) pos) pos) [ArgExpr ex2 pos] pos

        proc ex@(Call (Var (Ident str _) _) [ArgExpr ex1 _] pos)
            = mapExpressions proc $
                maybe' (Map.lookup str dunderFunctions) ex $ \dunder ->
                    Call (Dot ex1 (Ident dunder pos) pos) [] pos




        proc ex = mapExpressions proc ex
