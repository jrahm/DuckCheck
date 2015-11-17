{-# LANGUAGE RankNTypes #-}
module Hiss where

import Data.Set (Set)
import Hiss.Monad
import Hiss.Flags

import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3

import Language.Python.Common

import Control.Monad (foldM)
import Hiss.AST.Util

import Text.Printf
import Data.List

parsePython :: FilePath -> Hiss a (ModuleSpan, [Token])
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    fromEither $
        (if version2 then P2.parseModule else P3.parseModule) sourceCode fp

{- Collects and infers the type of a variable name over
 - the span of the list of statements given. -}
inferTypeForVariable :: forall e. String -> [Statement e] -> Hiss e StructuralType
inferTypeForVariable varname stmts =
        let expressions = concatMap walkExpressions stmts in
        foldl unionType emptyType <$> mapM observeExpr expressions
    where
        observeExpr (Dot (Var (Ident name _) _) (Ident attname _) _)
                     | name == varname = return (singletonType attname)
        observeExpr exp = foldl unionType emptyType <$> mapM observeExpr (childExpressions exp)


{- THis function will take a Python function and infer the type
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
        (Fun (Ident name _) _ _ _ _) -> do
            (args, ret) <- inferTypeForFunction stmt
            hlog $ printf "Type for %s: %s" name (intercalate " -> " $ map show (args ++ [ret]))
        _ -> return ()

runHissM :: FilePath -> Hiss SrcSpan ()
runHissM fp = do
    (Module stmts, _) <- parsePython fp
    mapM_ iterateAST stmts

runHissOnOneFile :: Set Flag -> FilePath -> IO ()
runHissOnOneFile flags file = runHissIO flags (runHissM file)
