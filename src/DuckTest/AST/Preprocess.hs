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
        proc (BinaryOp op@(In _) ex1 ex2 pos) =
            {- The in operator is backwards -}
            mapExpressions proc $
                Call (Dot ex2 (Ident (toDunderName op) pos) pos) [ArgExpr ex1 pos] pos

        proc (BinaryOp op ex1 ex2 pos) =
            {- Convert all binary operators to their actual function calls
             - to make things easier -}
            mapExpressions proc $
                Call (Dot ex1 (Ident (toDunderName op) pos) pos) [ArgExpr ex2 pos] pos

        proc ex@(Call (Var (Ident str _) _) [ArgExpr ex1 _] pos)
            {- Change known calls to dunder attributes. -}
            = mapExpressions proc $
                maybe' (Map.lookup str dunderFunctions) ex $ \dunder ->
                    Call (Dot ex1 (Ident dunder pos) pos) [] pos

        {- Expand out literals into functions -}
        proc ex@(Int _ _ pos)
            = Call (Var (Ident "int" pos) pos) [ArgExpr ex pos] pos

        proc ex@(Strings _ pos)
            = Call (Var (Ident "str" pos) pos) [ArgExpr ex pos] pos




        proc ex = mapExpressions proc ex
