{-# LANGUAGE TupleSections #-}
{- This module contains routines to construct and interpret the dependency graph
 - of a function between all the expressions of the function. -}

module DuckTest.Infer.DependencyGraph where

import Prelude hiding (exp)
import DuckTest.Internal.Common hiding (annot, comma, slices)
import DuckTest.Internal.State
import DuckTest.Types
import Data.Graph.Inductive

import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow

import Control.Monad.Trans.Either

import DuckTest.AST.Util
import Data.IORef
import qualified Data.Map as M

data Expression a = ExprExpression (Expr a) | ExprVariable String (Expression a)

data DependencyNode a =
        DependencyNode String (Maybe (Expr a)) (IORef (Maybe PyType))

data DependencyGraph a =
        DependencyGraph {
            internalGraph :: Gr (DependencyNode a) (),
            sinkNodes :: [Node],
            internalVariables :: Map String Node
        }

buildDependencyGraph ::
    [Statement a] ->
        EitherT (String, a) IO (DependencyGraph a)

buildDependencyGraph stmts =
    -- let refVars = map referencedVariables stmts
    --     nodeMap = foldl (\m (a, b) -> M.insert a b m) mempty (zip [0..] $ nub (concat refVars))
    {- I need to have a map of . -}
    let (mp, _) = execState (runFunction (flatten stmts)) (mempty, 0)
        contexts = M.elems mp
        sinks = mapMaybe (\(_, n, (str, _), _) -> case str of
                            "$return$" -> Just n
                            _ -> Nothing) contexts
        variables = fmap (\(_, n, _, _) -> n) mp
        in do
        internal <- (undir . buildGr) <$> mapM makeDependencyNode contexts
        return (DependencyGraph internal sinks variables)


    where
        makeDependencyNode (a, n, (str, expr), b) =
            (a, n, , b) <$> (DependencyNode str expr <$> lift (newIORef Nothing))

        runFunction :: [Statement a] -> State (Map String (Context (String, Maybe (Expr a)) ()), Int) ()
        runFunction statements =
            forM_ statements $ \stmt ->
                case stmt of
                    Assign [Var (Ident name _) _] expr _ ->  do

                        let referenced = referencedVariables expr

                        ref' <- forM referenced $ \ref -> do
                                ctx <- M.lookup ref . fst <$> get

                                case ctx of
                                    Nothing -> do
                                        node <- snd <$> get
                                        modify' (second $ (+1))
                                        modify' (first $ M.insert ref ([], node, (ref, Nothing), []))
                                        return (node :: Int)

                                    Just (_, node, _, _) ->
                                        return node


                        node <- snd <$> get
                        let ctx = ([], node, (name, Just expr), map ((),) ref')
                        modify' (first $ M.insert name ctx)
                        modify' (second $ (+1))

                    Return (Just expr) annot ->
                        runFunction [Assign [Var (Ident "$return$" annot) annot] expr annot]

                    _ -> return ()


referencedVariables :: (HasExpressions e) => e a -> [String]
referencedVariables stmt = execWriter $
                            walkExpressionM stmt $
                                \expr ->
                                    case expr of
                                        Var (Ident x _) _ -> tell [x] >> return Skip
                                        _ -> return Recurse

flatten :: [Statement a] -> [Statement a]
flatten s = evalState (flatten' s) 0

flatten' :: [Statement a] -> State Int [Statement a]
flatten' arr = execWriterT (mapM_ steamroll arr)
    where
        steamroll :: Statement a -> WriterT [Statement a] (State Int) ()
        steamroll stmt = (=<<) emit $
            case stmt of
                While cond body els annot ->
                    While <$> flattenExpr cond
                          <*> lift (flatten' body)
                          <*> lift (flatten' els)
                          <*> pure annot

                For targets generator body els a ->
                    For <$> mapM flattenExpr targets
                        <*> flattenExpr generator
                        <*> lift (flatten' body)
                        <*> lift (flatten' els)
                        <*> pure a

                Conditional guards els annot ->
                    Conditional <$> mapM (\(c, b) -> liftM2 (,) (flattenExpr c) (lift (flatten' b)) ) guards
                                <*> lift (flatten' els)
                                <*> pure annot

                Assign assignto expr annot ->
                    Assign assignto <$> flattenExpr expr <*> pure annot

                AugmentedAssign assignto op expression annot ->
                    AugmentedAssign assignto op <$> flattenExpr expression <*> pure annot

                Return expr annot ->
                    Return <$> mapM flattenExpr expr <*> pure annot

                Try body excepts els finally annot ->
                    Try <$> lift (flatten' body)
                        <*> mapM (\(Handler c s a) -> Handler c <$> lift (flatten' s) <*> pure a) excepts
                        <*> lift (flatten' els)
                        <*> lift (flatten' finally)
                        <*> pure annot

                With ctx body annot ->
                    With <$> mapM (\(expr, maybeexpr) -> (,) <$> flattenExpr expr <*> mapM flattenExpr maybeexpr) ctx
                         <*> lift (flatten' body)
                         <*> pure annot

                Delete list annot ->
                    Delete <$> mapM flattenExpr list <*> pure annot

                Assert exprs annot ->
                    Assert <$> mapM flattenExpr exprs
                           <*> pure annot

                Print ch exprs comma annot ->
                    Print ch <$> mapM flattenExpr exprs
                             <*> pure comma
                             <*> pure annot

                Pass {} -> return stmt

                Break {} -> return stmt

                Continue {} -> return stmt

                Decorated {} -> return stmt

                Class {} -> return stmt

                Fun {} -> return stmt

                Raise {} -> return stmt

                StmtExpr expr a ->
                    StmtExpr <$> flattenExpr expr <*> pure a

                _ -> return stmt


        flattenArgument :: Argument a -> WriterT [Statement a] (State Int) (Argument a)
        flattenArgument (ArgExpr expr annot) = ArgExpr <$> flattenExpr expr <*> pure annot
        flattenArgument (ArgVarArgsPos expr annot) = ArgVarArgsPos <$> flattenExpr expr <*> pure annot
        flattenArgument (ArgVarArgsKeyword expr annot) = ArgVarArgsKeyword <$> flattenExpr expr <*> pure annot
        flattenArgument (ArgKeyword keyword expr annot) = ArgKeyword keyword <$> flattenExpr expr <*> pure annot

        flattenExpr :: Expr a -> WriterT [Statement a] (State Int) (Expr a)
        flattenExpr expr =
            case expr of
                Call fun args annot ->
                    (=<<) mkVar $
                        Call <$> flattenExpr fun
                             <*> mapM flattenArgument args
                             <*> pure annot

                Subscript lhs subscriptexp annot ->
                    (=<<) mkVar $
                        Subscript <$> flattenExpr lhs
                                  <*> flattenExpr subscriptexp
                                  <*> pure annot

                SlicedExpr lhs slices annot ->
                    (=<<) mkVar $
                        SlicedExpr <$> flattenExpr lhs
                                   <*> pure slices -- todo this should probably be flattened as well
                                   <*> pure annot

                CondExpr true condition false annot ->
                    (=<<) mkVar $
                        CondExpr <$> flattenExpr true
                                 <*> flattenExpr condition
                                 <*> flattenExpr false
                                 <*> pure annot

                BinaryOp op lhs rhs annot ->
                    (=<<) mkVar $
                        BinaryOp op <$> flattenExpr lhs
                                    <*> flattenExpr rhs
                                    <*> pure annot

                UnaryOp op exp annot ->
                    (=<<) mkVar $
                        UnaryOp op <$> flattenExpr exp
                                   <*> pure annot

                Dot lhs attr annot ->
                    (=<<) mkVar $
                        Dot <$> flattenExpr lhs
                            <*> pure attr
                            <*> pure annot

                Lambda {} ->
                    mkVar expr

                Tuple exps annot ->
                    Tuple <$> mapM flattenExpr exps
                          <*> pure annot

                Yield arg annot ->
                    Yield <$>
                        mapM (\yield -> case yield of
                                YieldFrom exp ann -> YieldFrom <$> flattenExpr exp <*> pure ann
                                YieldExpr exp -> YieldExpr <$> flattenExpr exp) arg <*> pure annot

                -- TODO fill in the rest.
                _ -> return expr

        emit :: Statement a -> WriterT [Statement a] (State Int) ()
        emit = tell . return

        mkVar :: Expr a -> WriterT [Statement a] (State Int) (Expr a)
        mkVar expr = do
            let annot = expr_annot expr
            ident <- lift newVariable
            emit (Assign [Var (Ident ident annot) annot] expr annot)
            return (Var (Ident ident annot) annot)

        newVariable :: State Int String
        newVariable = (("v"++) <$> (show <$> get)) <* modify (+1)
