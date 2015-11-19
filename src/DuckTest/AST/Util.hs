module DuckTest.AST.Util where

import Language.Python.Common
import Data.Maybe
import Control.Monad.Writer.Lazy

walkAllExpressions :: [Statement a] -> [Expr a]
walkAllExpressions stmts =
    execWriter $ forM_ exps walk
    where
        exps = concatMap walkExpressions stmts

        walk :: Expr a -> Writer [Expr a] ()
        walk exp = do
              tell [exp]
              mapM_ walk $ childExpressions exp



childExpressions :: Expr a -> [Expr a]
childExpressions exp =
    case exp of
        Call fn args _ -> fn : map getExpression args
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



walkStatements :: Statement a -> [Statement a]
walkStatements stmt =
    case stmt of
        (Fun {fun_body = body}) -> stmt : concatMap walkStatements body
        (Class {class_body = body}) -> stmt : concatMap walkStatements body
        (While {while_body = body}) -> stmt : concatMap walkStatements body
        (For {for_body = body, for_else = el}) -> stmt : concatMap walkStatements body ++ concatMap walkStatements el
        _ -> return stmt -- todo add more

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

class HasIdentifier a where
     getIdentifier :: a -> Maybe String

class HasExpression a where
    getExpression :: a e -> Expr e

instance HasExpression Argument where
    getExpression (ArgExpr e _) = e
    getExpression (ArgVarArgsPos e _) = e
    getExpression (ArgVarArgsKeyword e _) = e
    getExpression (ArgKeyword _ e _) = e


instance HasIdentifier (Parameter a) where
    getIdentifier s = case s of
            Param (Ident n _) _ _ _ -> Just n
            VarArgsPos (Ident n _) _ _ -> Just n
            VarArgsKeyword (Ident n _) _ _ -> Just n
            _ -> Nothing

tryGetIdentifier :: (HasIdentifier a) => String -> a -> String
tryGetIdentifier str = fromMaybe str . getIdentifier

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap fn = mconcat . map fn

mconcatMapM :: (Monoid mo, Monad m) => (a -> m mo) -> [a] -> m mo
mconcatMapM fn lst = mconcat <$> mapM fn lst

mconcatMapMaybe :: (Monoid m) => (a -> Maybe m) -> [a] -> m
mconcatMapMaybe fn = mconcat . mapMaybe fn
