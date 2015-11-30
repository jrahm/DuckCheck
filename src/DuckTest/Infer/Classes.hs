module DuckTest.Infer.Classes where

import DuckTest.Internal.Common

import DuckTest.Monad
import DuckTest.AST.Util
import DuckTest.AST.BinaryOperators
import DuckTest.Types

import qualified Data.Map as Map

import DuckTest.Infer.Functions
import DuckTest.Internal.State

inferTypeForClass :: InternalState -> Statement a -> DuckTest a PyType
inferTypeForClass st cls@(Class {class_body = body, class_name = (Ident clname _)})
    = do
        let topFunctions = fromList $ mapMaybe isFunction body
        let selfAssignments = mconcatMap findSelfAssign (walkStatements cls)
        let retType = Scalar $ mappend topFunctions selfAssignments

        maybeInitType <- mapM (inferTypeForFunction st) (findInit body)
        let (Functional args _) = fromMaybe (Functional [] retType) maybeInitType

        return $ Functional args retType

        where isFunction (Fun {fun_name = (Ident name _)}) = Just name
              isFunction _ = Nothing

              findSelfAssign (Assign [Dot (Var (Ident "self" _) _) (Ident att _) _] _ _) = singletonType att
              findSelfAssign _ = emptyType

              findInit =  foldl (\x stmt -> case stmt of
                                  (Fun {fun_name = (Ident "__init__" _)}) ->
                                      Just stmt
                                  _ -> x) Nothing

