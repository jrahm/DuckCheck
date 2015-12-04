{-# LANGUAGE ScopedTypeVariables #-}
module DuckTest.Infer.Classes where

import DuckTest.Internal.Common hiding (union)

import DuckTest.Monad
import DuckTest.MonadHelper
import DuckTest.AST.Util
import DuckTest.AST.BinaryOperators
import DuckTest.Types

import qualified Data.Map as Map

import DuckTest.Infer.Functions
import DuckTest.Infer.Expression
import DuckTest.Internal.State
import DuckTest.Internal.Format

import Control.Arrow
import Debug.Trace

{- Takes a state and a list of statements (the class body) and finds
 - the self assignments. This returns a PyType as a scalar of all
 - the found assignments.
 -
 - Essentially, this function traces the __init__ function and
 - all functions called directly from it and looks for expressions
 - like self.x = expr. It then knows that if expr :: t then
 - self :: {x :: t} u tau (where tau is the rest of the type of self)
 -}
findSelfAssignments :: forall a. InternalState -> [Statement a] -> DuckTest a PyType
findSelfAssignments state statements =
    traceAssignments functionMap initFunction
    where
        {- A map of all function by their names to bodies.
         - String => [Statement] -}
        functionMap :: Map String [Statement a]
        functionMap = Map.fromList $ flip mapMaybe statements $ \stmt ->
                        case stmt of
                            (Fun {fun_name = (Ident name _), fun_body = body}) ->
                                Just (name, body)
                            _ -> Nothing

        {- The __init__ function. If it cannot be found, then we use
         - an empty function instead. -}
        initFunction :: [Statement a]
        initFunction = Map.findWithDefault [] "__init__" functionMap

        {- Does the actual fold. Traces over the init function and
         - all directly called functions. When an assignment is witnessed
         - we append the corresponding singleton type to the type.
         -
         - If we see a standalone function, then we trace into that function,
         - but we need to make sure to avoid infinite recursion ...
         -}
        traceAssignments :: Map String [Statement a] -> [Statement a] -> DuckTest a PyType
        traceAssignments map stmts =
            foldM' Void stmts  $ \typ stmt -> case stmt of

                (Assign [Dot (Var (Ident "self" _) _) (Ident att _) _] ex _) -> do
                    inferred <- inferTypeForExpression (addVariableType "self" typ state) ex
                    return (typ `union` singleton att inferred)

                (StmtExpr (Call (Dot (Var (Ident "self" _) _) (Ident att _) _) _ _) _) -> do
                    infer <- fromMaybe (return Void) $ traceAssignments (Map.delete att map) <$> Map.lookup att map
                    return (typ `union` infer)

                _ -> return typ

{- From a PyType, get the init function from it. If we cannot get the
 - init function from it, we simply create an itit function type. -}
initType :: PyType -> PyType
initType f@(Scalar _ m) =
    let init = Map.findWithDefault (Functional [] f) "__init__" m in
    case init of
        (Functional args _) -> Functional args f
        _ -> Functional [] f
initType f = Functional [] f

{- Given a instance type of a class, check to make sure
 - it is consistent with the types expected for `self`
 - in all other functions. -}
matchBoundWithStatic :: e -> PyType -> PyType -> DuckTest e ()
matchBoundWithStatic pos bound (Scalar _ m) =
    forM_ (map snd $ Map.toList m) $ \typ ->
        case typ of
            (Functional ((_, self):_) _) ->
                whenJust (matchType self bound)
                    (warnTypeError pos)
            _ -> return ()

{- Given self assignments (found from the above function) and the
 - sattic type of the class, create a bound type instance that
 - has the same functions as the static instance, but with the
 - `self' parameter removed.
 -}
toBoundType :: String -> PyType -> PyType -> PyType
toBoundType name (Scalar _ m2) selfAssign =
    let boundFns = Scalar Nothing $
                    flip Map.mapMaybe m2 $
                        \typ -> case typ of
                                 (Functional (_:as) r) -> Just (Functional as r)
                                 _ -> Nothing
        in
    selfAssign `union` boundFns
