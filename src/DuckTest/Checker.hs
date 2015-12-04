{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{- Module with the checker and iterate functions -}

module DuckTest.Checker where

import qualified Data.Map as Map
import DuckTest.Internal.Common hiding (union)
import DuckTest.Internal.State
import DuckTest.Internal.Format

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
runChecker init stmts = do
    Trace %%! duckf "Running checker on stmts"
    forM_ stmts $ \s -> Trace %%! duckf s
    foldM foldFunction init stmts

runChecker_ :: (CheckerState s) => s -> [Statement SrcSpan] -> DuckTest SrcSpan ()
runChecker_ a = void . runChecker a

instance CheckerState InternalState where
    foldFunction currentState statement = do
        Trace %%! duckf "check: " statement

        when (returnHit currentState) $
            warn (annot statement) $ duckf "Dead code"

        case statement of

            (Import [ImportItem dotted@(_:_) as pos] _) -> do
                let dottedpaths@(h:t) = map (\(Ident name _) -> name) dotted

                modType <- makeImport pos dottedpaths parsePython $ \stmts ->
                    stateToType <$> runChecker initState stmts

                maybe' modType (return currentState) $ \a -> do
                    Debug %%! duckf "Module " h " :: " a
                    case as of
                        Nothing ->
                            return $ addVariableType h (liftFromDotList t a) currentState
                        Just (Ident name _) ->
                            return $ addVariableType name a currentState

            (Return expr pos) ->
                if returnHit currentState then
                    return currentState
                    else do
                    inferred <- maybe' expr (return Void) (inferTypeForExpression currentState)
                    return (setReturnType inferred currentState)

            (Fun {fun_name = (Ident name _), fun_body = body}) -> do
                {- For functions, we infer the type and recursively check the
                 - function body for errors. -}
                 !functionInferredType <- inferTypeForFunction currentState statement
                 let newstate = addVariableType name functionInferredType currentState
                 case functionInferredType of
                    (Functional args _) -> do
                       ret <- getReturnType <$> runChecker (addAll args newstate) body
                       let newfntype = Functional args ret
                       Info %%! duckf "\n(Inferred) " name " :: " newfntype "\n"
                       return $ addVariableType name newfntype currentState
                    _ -> do
                        Warn %% "This should not happen, infer type of function returned a type that isn't a function."
                        return currentState

            ex@(Class {class_name = (Ident name _), class_body = body}) -> do
                staticVarsState <- foldM' mempty body $ \state stmt ->
                    case stmt of
                        (Assign [Var (Ident vname _) _] ex pos) -> do
                             inferredType <- inferTypeForExpression currentState ex
                             return $ addVariableType vname inferredType state
                        _ -> return state

                let staticVarType = stateToType staticVarsState
                let newstate = addVariableType name staticVarType currentState
                staticClassState <- runChecker newstate $ mapMaybe (\stmt ->
                                      case stmt of
                                        (Assign {}) -> Nothing
                                        _ -> Just stmt) body
                {- The static type of the class. -}
                let staticClassType@(Scalar _ m) = staticVarType `union` stateToType staticClassState
                let newstate = addVariableType name staticClassType currentState

                return newstate


            (Assign [Var (Ident vname _) _] ex pos) -> do
                inferredType <- inferTypeForExpression currentState ex
                when (isVoid inferredType) $
                    warn pos $ duckf "Void type not ignored as it ought to be!"
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
