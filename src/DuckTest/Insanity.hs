{- This module is dedicated to detecting insanity for
 - syntax trees. -}

{-# LANGUAGE TupleSections #-}

module DuckTest.Insanity where

import DuckTest.Internal.Common

import DuckTest.Types
import DuckTest.Monad
import DuckTest.MonadHelper

import qualified Data.Map as Map
import qualified Data.Set as Set

import DuckTest.AST.Util
import DuckTest.Builtins
import DuckTest.AST.BinaryOperators
import DuckTest.Checker


detectInsanityForFunction :: Map String StructuralType -> Statement a -> DuckTest a ()
detectInsanityForFunction curmap (Fun {fun_name = Ident name _, fun_body = body, fun_args = args}) =
    do
       fn <- getFunction name

       case fn of

        Nothing ->
            verbose $ printf "Function magically appeared: %s" name

        Just (Function _ (paramTypes, _)) -> do

            let argNamesAndTypes :: [Maybe (String, StructuralType)]
                argNamesAndTypes = zipWith (\arg typ ->
                                            (,typ) <$> getIdentifier arg)
                                              args paramTypes

                initmap = Map.fromList (catMaybes argNamesAndTypes) `Map.union` curmap

            verbose $ "Recursively check function " ++ name ++ ". Variable map: " ++ show initmap
            runChecker detectInsanity initmap body

detectInsanity :: Checker (Map String StructuralType) a
detectInsanity initmap b = do
    verbose "!!!! Insanity Detection Phase !!!!"
    verbose (show initmap)
    void $ foldM detect initmap b

    where
        detect :: Map String StructuralType -> Statement a -> DuckTest a (Map String StructuralType)
        detect db stmt = do
            verbose $ printf "\x1b[01;32m%s\x1b[00m" (prettyText stmt)
            case stmt of

                {- Handle the case where we are assigning to a function we
                 - know the type of! -}
                (Assign [Var (Ident vname _) _] (Call (Var (Ident fn _) _) _ _) pos) ->
                 do
                    verbose "\x1b[01;33mHandled by case 1\x1b[00m"

                    f <- getFunction fn
                    typ <- case f of
                               Just (Function _ (_, returnType)) -> return returnType
                               Nothing -> do
                                emitWarning ("Possible unknown global function " ++ fn) pos
                                return emptyType

                    verbose $ "Assign to variable named " ++ vname ++ " type " ++ show typ ++ "(from function " ++ fn ++ ")"
                    return (Map.insert vname typ db)

                {- Literal string assignment. -}
                (Assign [Var (Ident vname _) _] (Strings {}) _) -> do
                    verbose "\x1b[01;33mHandled by case 2\x1b[00m"

                    verbose $ prettyText stmt
                    return $ Map.insert vname (toStructuralType strClass) db

                {- General catch all assignment code to shut up
                 - about undefined variables. Simply collect
                 - all the types. Don't worry about trying to
                 - infer the type -}
                (Assign vars _ _) -> do
                    verbose "\x1b[01;33mHandled by case 3\x1b[00m"
                    return $
                        foldl (\m exp ->
                                case exp of
                                    (Var (Ident vname _) _) -> Map.insert vname emptyType m
                                    _ -> m) db vars

                {- Handle insanity for a specific function. This function called
                 - will correctly set the arguments to the correct types
                 - to be used later. -}
                (Fun {}) -> do
                    verbose "\x1b[01;33mHandled by case 4\x1b[00m"

                    detectInsanityForFunction db stmt
                    return db

                (Class {class_body = body, class_name = Ident name _}) -> do
                    verbose "\x1b[01;33mHandled by case 5\x1b[00m"

                    -- TODO this will cause some problems with name clashes
                    -- between global and local scope.
                    underContext name $ detectInsanity Map.empty body
                    return db

                (Conditional guards elsest _) -> do
                    verbose "\x1b[01;33mHandled by case 6\x1b[00m"

                    verbose "Entering conditional statement!"
                    forM_ guards $ \(expr, body) -> do
                        _ <- detectExp db expr
                        detectInsanity db body

                    detectInsanity db elsest
                    return db

                stmt -> do
                    verbose "\x1b[01;31mUnhandled\x1b[00m"
                    foldM detectExp db $ recursivelyWalkExpressions [stmt]

        detectExp :: Map String StructuralType -> Expr a -> DuckTest a (Map String StructuralType)
        detectExp db exp = case exp of
            (Dot (Var (Ident varname _) pos) (Ident attname _) _) ->

                case Map.lookup varname db of

                    Nothing -> do
                        let warning = printf "Possible undefined variable: %s" varname
                        emitWarning warning pos
                        return db

                    Just typ -> do
                        unless (typeHasAttr typ attname) $
                            let warning = printf "Probable attribute error: %s has no attribute %s" varname attname in
                            emitWarning warning pos
                        return db

            (BinaryOp op (Var (Ident vname pos) _) _ _) -> do
                maybe' (Map.lookup vname db)
                    (possibleUndefinedError vname pos) $
                    \typ ->
                        unless (typeHasAttr typ $ toDunderName op) $
                            possibleAttributeError vname (toDunderName op) pos
                return db

            (Call (Var (Ident fnname _) _) args pos) -> do
                fn <- getGlobalFunction fnname
                maybe' fn (possibleUnknownGlobalFunction fnname pos) $
                    \(Function _ (argTypes, _)) -> do
                        inferredArgTypes <- forM args $ \arg ->
                            case arg of
                                ArgExpr (Var (Ident name _) pos) _ ->
                                    maybe' (Map.lookup name db) (do
                                        verbose $ name ++ " not found in " ++ show db
                                        possibleUndefinedError name pos
                                        return (emptyType, pos)) (return . (,pos))

                                _ -> return (emptyType, pos)

                        zipWithM_ (\(t1, pos) t2 ->
                            unless (isCompatibleWith t1 t2) $
                                emitTypeWarning t1 t2 fnname pos) inferredArgTypes argTypes
                return db

            _ -> return db

        emitTypeWarning t1 t2 fn =
            emitWarning $
                printf "Probable Attribute Error: The type %s does not have the attribute(s): %s (needed by %s)"
                    (getTypeName t1)
                    (intercalate ", " (Set.toList $ typeDifference t2 t1))
                    fn
