{-# LANGUAGE MultiParamTypeClasses #-}
{- Module with the checker and iterate functions -}

module DuckTest.Checker where

import qualified Data.Map as Map
import DuckTest.Internal.Common
import DuckTest.Internal.State

import DuckTest.Monad
import DuckTest.Infer.Functions
import DuckTest.Infer.Expression
import DuckTest.Infer.Classes
import DuckTest.Types
import DuckTest.MonadHelper

import DuckTest.AST.Util
import DuckTest.Parse

import DuckTest.Builtins

class CheckerState s where
    {- The function used in a monadic fold across a list of
     - statements -}
    foldFunction :: s -> Statement SrcSpan -> DuckTest SrcSpan s

runChecker :: (CheckerState s) => s -> [Statement SrcSpan] -> DuckTest SrcSpan s
runChecker = foldM foldFunction


runChecker_ :: (CheckerState s) => s -> [Statement SrcSpan] -> DuckTest SrcSpan ()
runChecker_ a = void . runChecker a

instance CheckerState InternalState where

    foldFunction currentState statement = case statement of

        (Import [ImportItem dotted@(_:_) as pos] _) -> do
            let dottedpaths@(h:t) = map (\(Ident name _) -> name) dotted

            modType <- makeImport pos dottedpaths parsePython $ \stmts ->
                stateToType <$> runChecker initState stmts

            maybe' modType (return currentState) $ \a ->
                return $ addVariableType h (liftFromDotList t a) currentState


        (Fun {fun_name = (Ident name _), fun_body = body}) -> do
            {- For functions, we infer the type and recursively check the
             - function body for errors. -}
             functionInferredType <- inferTypeForFunction currentState statement
             let newstate = addVariableType name functionInferredType currentState
             case functionInferredType of
                (Functional args _) ->
                    runChecker_ (addAll args newstate) body
                _ ->
                    Warn %% "This should not happen, infer type of function returned a type that isn't a function."
             return newstate

        (Class {class_name = (Ident name _), class_body = body}) -> do
            classFunctionalType <- inferTypeForClass currentState statement
            Debug %% printf "\nClass added and has type ::\n%s\n " (prettyType classFunctionalType)
            let newstate = addVariableType name classFunctionalType currentState
            return newstate

        (Assign {assign_to=[Var (Ident vname _) _], assign_expr=ex}) -> do
            inferredType <- inferTypeForExpression currentState ex
            return $ addVariableType vname inferredType currentState

        (Conditional {cond_guards=guards, cond_else=elsebody}) -> do
            endStates <- forM guards $ \(expr, stmts) -> do
                          _ <- inferTypeForExpression currentState expr
                          runChecker currentState stmts

            elseState <- runChecker currentState elsebody
            let intersect = foldl intersectStates elseState endStates
            return intersect

        _ -> do
             mapM_ (inferTypeForExpression currentState) (subExpressions statement)
             return currentState
