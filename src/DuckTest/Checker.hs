{- Module with the checker and iterate functions -}

module DuckTest.Checker where

import DuckTest.Internal.Common

import DuckTest.Monad
import DuckTest.Infer.Functions
import DuckTest.Infer.Classes
import DuckTest.Types

iterateAST :: Statement a -> DuckTest a ()
iterateAST stmt =
    case stmt of
        (Fun {fun_name=(Ident name _)}) -> do
            typ@(FunctionType args ret) <- inferTypeForFunction stmt
            Debug %% printf "Type for %s: %s" name (typeToString typ)
            addFunction (Function name typ)

        (Class {class_name=(Ident name _)}) -> do
            (clsType, members) <- mkClass stmt
            Debug %% printf "Class %s. Type: %s" name (show clsType)
            addClass (HClass name clsType members)

        _ -> return ()

type Checker m a = m -> [Statement a ] -> DuckTest a()

runChecker ::  Checker m a -> m -> [Statement a] -> DuckTest a ()
runChecker checker initmap stmts =
    saveState $ do
        mapM_ iterateAST stmts
        checker initmap stmts

