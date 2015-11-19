{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module DuckTest where

import Control.Monad (foldM, when, unless, void, forM, zipWithM_)
import Data.Foldable (mapM_, forM_)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import DuckTest.AST.Util
import DuckTest.Flags
import DuckTest.Monad
import DuckTest.Infer.Functions
import Language.Python.Common
import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3
import Prelude hiding (mapM_)
import System.IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

import DuckTest.Builtins
import DuckTest.Types

parsePython :: FilePath -> DuckTest SrcSpan (Maybe (ModuleSpan, [Token]))
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    case (if version2 then P2.parseModule else P3.parseModule) sourceCode fp of
        Left (UnexpectedToken token) -> emitWarning "ParseError: Unexpected token" (token_span token) >> return Nothing
        Left (UnexpectedChar ch NoLocation) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) SpanEmpty >> return Nothing
        Left (UnexpectedChar ch (Sloc f l c)) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) (SpanPoint f l c) >> return Nothing
        Left (StrError str) -> emitWarning ("ParseError: %s" ++ str) SpanEmpty >> return Nothing
        Right a -> return (Just a)


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
            runChecker initmap body


detectInsanity :: Map String StructuralType -> [Statement a] -> DuckTest a ()
detectInsanity initmap b = do
    verbose "!!!! Insanity Detection Phase !!!!"
    verbose (show initmap)
    void $ foldM detect initmap b

    where
        detect :: Map String StructuralType -> Statement a -> DuckTest a (Map String StructuralType)
        detect db stmt =
            case stmt of

                {- Handle the case where we are assigning to a function we
                 - know the type of! -}
                (Assign [Var (Ident vname _) _] (Call (Var (Ident fn _) _) _ _) pos) ->
                 do
                    f <- getFunction fn
                    typ <- case f of
                               Just (Function _ (_, returnType)) -> return returnType
                               Nothing -> do
                                emitWarning ("Possible unknown global function " ++ fn) pos
                                return emptyType
                    verbose $ "Assign to variable named " ++ vname
                    return (Map.insert vname typ db)

                {- Literal string assignment. -}
                (Assign [Var (Ident vname _) _] (Strings {}) _) -> do
                    verbose $ prettyText stmt
                    return $ Map.insert vname (toStructuralType strClass) db

                {- General catch all assignment code to shut up
                 - about undefined variables. Simply collect
                 - all the types. Don't worry about trying to
                 - infer the type -}
                (Assign vars _ _) -> return $
                    foldl (\m exp ->
                            case exp of
                                (Var (Ident vname _) _) -> Map.insert vname emptyType m
                                _ -> m) db vars

                {- Handle insanity for a specific function. This function called
                 - will correctly set the arguments to the correct types
                 - to be used later. -}
                (Fun {}) -> do
                    detectInsanityForFunction db stmt
                    return db

                (Class {class_body = body, class_name = Ident name _}) -> do
                    -- TODO this will cause some problems with name clashes
                    -- between global and local scope.
                    underContext name $ detectInsanity Map.empty body
                    return db

                (Conditional guards elsest _) -> do
                    verbose "Entering conditional statement!"
                    forM_ guards $ \(expr, body) -> do
                        _ <- detectExp db expr
                        detectInsanity db body

                    detectInsanity db elsest
                    return db

                (StmtExpr expr _) -> detectExp db expr
                (Return (Just expr) _) -> detectExp db expr

                stmt -> do
                    verbose $ "Unhandled: " ++ prettyText stmt
                    foldM detectExp db $ walkAllExpressions [stmt]

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
            (Call (Var (Ident fnname _) _) args pos) -> do
                fn <- getGlobalFunction fnname
                case fn of
                    Nothing -> do
                        emitWarning ("Possible unknown global function " ++ fnname) pos
                        return db
                    Just (Function _ (argTypes, _)) -> do
                        inferredArgTypes <- forM args $ \arg ->
                            case arg of
                                ArgExpr (Var (Ident name _) pos) _ ->
                                    maybe (do
                                        verbose $ name ++ " not found in " ++ show db
                                        emitWarning ("Possible undefined variable " ++ name) pos
                                        return (emptyType, pos)) (return . (,pos)) $ Map.lookup name db
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


iterateAST :: Statement a -> DuckTest a ()
iterateAST stmt =
    case stmt of
        (Fun {fun_name=(Ident name _)}) -> do
            typ@(args, ret) <- inferTypeForFunction stmt
            verbose $ printf "Type for %s: %s" name (typeToString typ)
            addFunction (Function name typ)

        (Class {class_name=(Ident name _)}) -> do
            (clsType, members) <- mkClass stmt
            verbose $ printf "Class %s. Type: %s" name (show clsType)
            addClass (HClass name clsType members)

        _ -> return ()

runChecker :: Map String StructuralType -> [Statement a] -> DuckTest a ()
runChecker initmap stmts =
    saveState $ do
        mapM_ iterateAST stmts
        detectInsanity initmap stmts

runDuckTestM :: FilePath -> DuckTest SrcSpan ()
runDuckTestM fp = do
    mayStmts <- parsePython fp
    case mayStmts of
        Just (Module stmts, _) -> runChecker Map.empty stmts
        Nothing -> return ()

getStartPos :: SrcSpan -> Maybe (String, Int, Int)
getStartPos sp = case sp of
    (SpanCoLinear fn r c _) -> Just (fn, r, c)
    (SpanMultiLine fn r c _ _) -> Just (fn, r, c)
    (SpanPoint fn r c) -> Just (fn, r, c)
    _ -> Nothing

runDuckTestOnOneFile :: Set Flag -> FilePath -> IO ()
runDuckTestOnOneFile flags file = do
    st <- runDuckTestIO flags (runDuckTestM file)
    forM_ (getWarnings st) $ \(err, pos) ->
        mapM_ (\(f, r, c) ->
            hPutStr stderr $ printf "%s(%d:%d): %s\n" f r c err) (getStartPos pos)
