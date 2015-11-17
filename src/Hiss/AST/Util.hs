module Hiss.AST.Util where

import Language.Python.Common
import Data.Maybe

childExpressions :: Expr a -> [Expr a]
childExpressions exp =
    case exp of
        Call fn args _ -> fn : map arg2Expr args
        Subscript e1 e2 _ -> [e1, e2]
        SlicedExpr e1 _ _ -> return e1 -- todo implement strides
        CondExpr e1 e2 e3 _ -> [e1, e2, e3]
        BinaryOp _ e1 e2 _ -> [e1, e2]
        UnaryOp _ e1 _ -> return e1
        Lambda _ body _ -> return body
        Tuple exprs _ -> exprs

        Yield (Just (YieldFrom expr _)) _ -> return expr
        Yield (Just (YieldExpr expr)) _ -> return expr
        Yield Nothing _ -> []

        Generator _ _ -> [] -- todo implement these
        ListComp _ _ -> []
        DictComp _ _ -> []
        SetComp _ _ -> []

        Dictionary mappings _ -> concatMap (\(DictMappingPair e1 e2) -> [e1, e2]) mappings
        Set exprs _ -> exprs
        Starred exp _ -> return exp
        Paren exp _ -> return exp
        StringConversion exp _ -> return exp

        Dot exp _ _ -> return exp
        List exps _ -> exps

        _ -> []



    where
        arg2Expr :: Argument a -> Expr a
        arg2Expr (ArgExpr e _) = e
        arg2Expr (ArgVarArgsPos e _) = e
        arg2Expr (ArgVarArgsKeyword e _) = e
        arg2Expr (ArgKeyword _ e _) = e

walkExpressions :: Statement a -> [Expr a]
walkExpressions stmt =
    case stmt of
        While condition body elsestmt _ ->
            condition : (getExps body ++
                         getExps elsestmt)

        For targets generator body elsestmt _ ->
            targets ++ [generator] ++ getExps body
                                   ++ getExps elsestmt

        Fun _ _ res body _ ->
            maybeToList res ++ getExps body

        Class _ _ body _ ->
            getExps body

        Conditional guards elsestmt _ ->
            concatMap (\(ex, suite) -> ex : getExps suite) guards ++
            getExps elsestmt

        Assign to from _ -> to ++ [from]

        AugmentedAssign to _ from _ -> [to, from]

        Return exp _ -> maybeToList exp

        Try body handlers elses finally _ ->
            getExps body ++ concatMap (\(Handler _ body _) -> getExps body) handlers ++
            getExps elses ++ getExps finally

        Raise exp _ -> [] -- todo implement this
        With context body _ ->
            concatMap (\(ex1, mayex2) -> ex1 : maybeToList mayex2) context ++ getExps body

        Delete exp _ -> exp
        StmtExpr exp _ -> return exp

        Assert exp _ -> exp
        Print _ exp _ _ -> exp

        Exec exp _ _ -> return exp -- todo implement

        _ -> []

    where
        getExps :: [Statement a] -> [Expr a]
        getExps = concatMap walkExpressions

