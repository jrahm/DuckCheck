module DuckTest.Infer.Classes where

import Language.Python.Common
import DuckTest.Monad
import DuckTest.AST.Util
import DuckTest.AST.BinaryOperators

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Printf
import DuckTest.Infer.Functions

mkClass :: Statement a -> DuckTest a (StructuralType, Map String Function)
mkClass clazz@(Class {class_body = body, class_name = (Ident clname _)}) = do
    functions <- topFunctions -- the functions in the body
    let initFn = Map.lookup "__init__" functions
    let structuralType =
         setTypeName clname $
             selfAssignments `unionType`
                fromSet (Map.keysSet functions)

    forM_ initFn $ \(Function _ (args, _)) -> do
        {- Add the init function to the global scope
         - as the name of the class -}
        let typ = (args, structuralType)
        verbose $ printf "__init__ added as %s with type %s" clname (typeToString typ)
        addFunction (Function clname typ)

    return (structuralType, Map.map (remapFirst structuralType) functions)

    where
        remapFirst clazzType (Function nm (args, ret)) =
            Function nm (clazzType : tail args, ret)

        topFunctions = foldM (\map stmt ->
                        case stmt of
                          (Fun {fun_name = (Ident name _)}) -> do
                            t <- inferTypeForFunction stmt
                            return $ Map.insert name (Function name t) map
                          _ -> return map
                          ) Map.empty body

        selfAssignments :: StructuralType
        selfAssignments = mconcatMap inferType (walkStatements clazz)

        inferType :: Statement a -> StructuralType
        inferType (Assign [Dot (Var (Ident "self" _) _) (Ident att _) _] _ _) = singletonType att
        inferType _ = emptyType
