{-# LANGUAGE RankNTypes #-}
module Hiss where

import Prelude hiding (mapM_)

import Data.Set (Set)
import qualified Data.Set as Set
import Hiss.Monad
import Hiss.Flags

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

import Language.Python.Common

import Control.Monad (foldM, when, unless)
import Data.Foldable (mapM_, forM_)
import Hiss.AST.Util

import Text.Printf
import Data.List

parsePython :: FilePath -> Hiss a (ModuleSpan, [Token])
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    fromEither $
        (if version2 then P2.parseModule else P3.parseModule) sourceCode fp

builtins :: Set.Set String
builtins = Set.fromList ["print"]

{- Collects and infers the type of a variable name over
 - the span of the list of statements given. -}
inferTypeForVariable :: forall e. String -> [Statement e] -> Hiss e StructuralType
inferTypeForVariable varname stmts =
        let expressions = concatMap walkExpressions stmts in
        foldl unionType emptyType <$> mapM observeExpr expressions
    where
        observeExpr (Dot (Var (Ident name _) _) (Ident attname _) _)
                     | name == varname =
                        verbose ("Found attribute usage: " ++ attname) >>
                        return (singletonType attname)

        observeExpr ex@(Call (Var (Ident fnname _) pos) args _) = do
                     mfn <- getFunction fnname
                     case mfn of
                        Nothing -> do

                            unless (fnname `Set.member` builtins) $
                                emitWarning ("Possible unknown function " ++ fnname) pos

                            iterateOverChildren ex

                        Just (Function _ (paramsType, _)) -> do
                            when (length args > length paramsType) $
                                emitWarning ("Possible too many arguments for " ++ fnname) pos

                            let inferTypeFromArguments current (expr', exprType) =
                                    let expr = arg2Expr expr' in
                                    case expr of
                                        (Var (Ident nm _) _) | nm == varname ->
                                            return $ unionType current exprType
                                        _ -> unionType current <$> observeExpr expr

                            foldM inferTypeFromArguments emptyType (zip args paramsType)

        observeExpr exp = iterateOverChildren exp

        iterateOverChildren exp = foldl unionType emptyType <$> mapM observeExpr (childExpressions exp)


{- This function will take a Python function and infer the type
 - of this function. The type infered from this function is
 - of the type [args] -> return type. All the types are in a
 - structural format -}
inferTypeForFunction :: Statement a -> Hiss a ([StructuralType], StructuralType)
inferTypeForFunction (Fun _ params _ body _) =
    let paramNames :: [String]
        paramNames = map (\(Param (Ident n _) _ _ _) -> n) params
        in do

        argTypes <- mapM (`inferTypeForVariable`body) paramNames
        let returnType = emptyType
        return (argTypes, returnType)

inferTypeForFunction _ = die "inferTypeForFunction called on non function!"


iterateAST :: Statement a -> Hiss a ()
iterateAST stmt =
    case stmt of
        (Fun {fun_name=(Ident name _)}) -> do
            typ@(args, ret) <- inferTypeForFunction stmt
            verbose $ printf "Type for %s: %s" name (intercalate " -> " $ map show (args ++ [ret]))
            addFunction (Function name typ)

        _ -> return ()

runHissM :: FilePath -> Hiss SrcSpan ()
runHissM fp = do
    (Module stmts, _) <- parsePython fp
    mapM_ iterateAST stmts

getStartPos :: SrcSpan -> Maybe (String, Int, Int)
getStartPos sp = case sp of
    (SpanCoLinear fn r c _) -> Just (fn, r, c)
    (SpanMultiLine fn r c _ _) -> Just (fn, r, c)
    (SpanPoint fn r c) -> Just (fn, r, c)
    _ -> Nothing

runHissOnOneFile :: Set Flag -> FilePath -> IO ()
runHissOnOneFile flags file = do
    st <- runHissIO flags (runHissM file)
    forM_ (getWarnings st) $ \(err, pos) ->
        mapM_ (\(f, r, c) -> printf "%s(%d,%d): %s\n" f r c err) (getStartPos pos)
