{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Hiss where

import Control.Monad (foldM, when, unless, void, forM, zipWithM_)
import Data.Foldable (mapM_, forM_)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Hiss.AST.Util
import Hiss.Flags
import Hiss.Monad
import Language.Python.Common
import Language.Python.Version2.Parser as P2
import Language.Python.Version3.Parser as P3
import Prelude hiding (mapM_)
import System.IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

parsePython :: FilePath -> Hiss SrcSpan (Maybe (ModuleSpan, [Token]))
parsePython fp = do
    version2 <- isVersion2
    sourceCode <- hissLiftIO (readFile fp)
    case (if version2 then P2.parseModule else P3.parseModule) sourceCode fp of
        Left (UnexpectedToken token) -> emitWarning "ParseError: Unexpected token" (token_span token) >> return Nothing
        Left (UnexpectedChar ch NoLocation) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) SpanEmpty >> return Nothing
        Left (UnexpectedChar ch (Sloc f l c)) -> emitWarning ("ParseError: Unexpected char " ++ [ch]) (SpanPoint f l c) >> return Nothing
        Left (StrError str) -> emitWarning ("ParseError: %s" ++ str) SpanEmpty >> return Nothing
        Right a -> return (Just a)


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
                {- This function may not exist, we should warn, but
                 - then continue on with the type inference -}
                Nothing ->

                    iterateOverChildren ex

                {- This function does exist, so let us continue using
                 - normal type-checking routines -}
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

        {- infer through the child expressions of this expression -}
        iterateOverChildren exp = foldl unionType emptyType <$>
                                    mapM observeExpr (childExpressions exp)


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

mkClass :: Statement a -> Hiss a (StructuralType, Map String Function)
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
        selfAssignments =
            foldl unionType emptyType $ mapMaybe inferType $ walkStatements clazz

        inferType :: Statement a -> Maybe StructuralType
        inferType (Assign [Dot (Var (Ident "self" _) _) (Ident att _) _] _ _) =
                        Just (singletonType att)
        inferType _ = Nothing

detectInsanityForFunction :: Statement a -> Hiss a ()
detectInsanityForFunction (Fun {fun_name = Ident name _, fun_body = body, fun_args = args}) =
    do
       fn <- getFunction name

       when (isNothing fn) $
        die $ printf "Function magically appeared: %s" name

       forM_ fn $ \(Function _ (paramTypes, _)) -> do
        let zipped :: [Maybe (String, StructuralType)]
            zipped = zipWith paramZip args paramTypes

            initmap = Map.fromList $ catMaybes zipped
        verbose $ "Detect insanity for: " ++ name
        detectInsanity initmap body

    where paramZip arg typ =
            case arg of
                Param {param_name = Ident name _} -> Just (name, typ)
                VarArgsPos {param_name = Ident name _} -> Just (name, typ)
                VarArgsKeyword {param_name = Ident name _} -> Just (name, typ)
                _ -> Nothing


detectInsanity :: Map String StructuralType -> [Statement a] -> Hiss a ()
detectInsanity initmap b = do
    verbose "!!!! Insanity Detection Phase !!!!"
    verbose (show initmap)
    void $ foldM detect initmap b

    where
        detect :: Map String StructuralType -> Statement a -> Hiss a (Map String StructuralType)
        detect db stmt =
            case stmt of

                {- Handle the case where we are assigning to a function we
                 - know the type of! -}
                (Assign [Var (Ident vname _) _] (Call (Var (Ident fn _) _) _ _) _) ->
                 do
                    f <- getFunction fn
                    let typ = case f of
                               Just (Function _ (_, returnType)) -> returnType
                               Nothing -> emptyType
                    verbose $ "Assign to variable named " ++ vname
                    return (Map.insert vname typ db)

                {- General cach all assignment code to shut up
                 - about undefined variables -}
                (Assign vars _ _) -> return $
                    foldl (\m exp ->
                            case exp of
                                (Var (Ident vname _) _) -> Map.insert vname emptyType m
                                _ -> m) db vars

                {- Handle insanity for a specific function. This function called
                 - will correctly set the arguments to the correct types
                 - to be used later. -}
                (Fun {}) -> do
                    detectInsanityForFunction stmt
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

        detectExp :: Map String StructuralType -> Expr a -> Hiss a (Map String StructuralType)
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


iterateAST :: Statement a -> Hiss a ()
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

runHissM :: FilePath -> Hiss SrcSpan ()
runHissM fp = do
    mayStmts <- parsePython fp
    case mayStmts of
        Just (Module stmts, _) -> do
            mapM_ iterateAST stmts
            detectInsanity Map.empty stmts
        Nothing -> return ()

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
        mapM_ (\(f, r, c) ->
            hPutStr stderr $ printf "%s(%d:%d): %s\n" f r c err) (getStartPos pos)
