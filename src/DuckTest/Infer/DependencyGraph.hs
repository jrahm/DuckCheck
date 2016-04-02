{- This module contains routines to construct and interpret the dependency graph
 - of a function between all the expressions of the function. -}

module DuckTest.Infer.DependencyGraph where

import DuckTest.Internal.Common
import DuckTest.Types
import Data.Graph.Inductive
import Control.Arrow

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Extra

data Expression a = ExprExpression (Expr a) | ExprVariable String (Expression a)

buildDependencyGraph ::
    [Statement a] ->
        Either (String, a) (Gr (Expression a, Maybe PyType) ())

buildDependencyGraph = undefined

flatten :: [Statement a] -> State Int [Statement a]
flatten arr = execWriterT (mapM_ steamroll arr)
    where
        steamroll :: Statement a -> WriterT [Statement a] (State Int) ()
        steamroll stmt = (=<<) emit $
            case stmt of
                While cond body els annot ->
                    While <$> flattenExpr cond
                          <*> lift (flatten body)
                          <*> lift (flatten els)
                          <*> pure annot

                For targets generator body els a ->
                    For <$> mapM flattenExpr targets
                        <*> flattenExpr generator
                        <*> lift (flatten body)
                        <*> lift (flatten els)
                        <*> pure a

                Conditional guards els annot ->
                    Conditional <$> mapM (\(c, b) -> liftM2 (,) (flattenExpr c) (lift (flatten b)) ) guards
                                <*> lift (flatten els)
                                <*> pure annot

                Assign assignto expr annot ->
                    Assign assignto <$> flattenExpr expr <*> pure annot

                AugmentedAssign assignto op expression annot ->
                    AugmentedAssign assignto op <$> flattenExpr expression <*> pure annot

                Return expr annot ->
                    Return <$> mapM flattenExpr expr <*> pure annot

                Try body excepts els finally annot ->
                    Try <$> lift (flatten body)
                        <*> mapM (\(Handler c s a) -> Handler c <$> lift (flatten s) <*> pure a) excepts
                        <*> lift (flatten els)
                        <*> lift (flatten finally)
                        <*> pure annot

                With ctx body annot ->
                    With <$> mapM (\(expr, maybeexpr) -> (,) <$> flattenExpr expr <*> mapM flattenExpr maybeexpr) ctx
                         <*> lift (flatten body)
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



        flattenExpr :: Expr a -> WriterT [Statement a] (State Int) (Expr a)
        flattenExpr = undefined

        emit :: Statement a -> WriterT [Statement a] (State Int) ()
        emit = tell . return

        newVariable :: State Int String
        newVariable = (("v"++) <$> (show <$> get)) <* modify (+1)
