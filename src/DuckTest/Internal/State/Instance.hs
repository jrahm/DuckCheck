{-| A module that is the implementation of the CheckerState class
    for the InternalState class -}
module DuckTest.Internal.State.Instance where

import DuckTest.Checker
import DuckTest.Internal.Common hiding (union)
import DuckTest.Internal.State
import DuckTest.Internal.Format

import DuckTest.Monad
import DuckTest.Infer.Functions
import DuckTest.Infer.Expression
import DuckTest.Infer.Classes
import DuckTest.Types

import DuckTest.AST.Util
import DuckTest.Parse

import DuckTest.Builtins

handleImport :: InternalState -> (String, [String]) -> Maybe String -> SrcSpan -> DuckTest SrcSpan InternalState
{-| Handle the observation of an import statement. This relies
 - on the DuckTest monad to pull in the correct module into its
 - state. We then check the module and lift it into its own object-}
handleImport state (h, t) as pos =  do
    modType <- ignore $ makeImport pos (h:t) parsePython $ \stmts ->
        stateToType <$> runChecker initState stmts

    maybe' modType (return state) $ \a -> do
        Debug %%! duckf "Module " h " :: " a

        case as of

            Nothing ->
                return $ addVariableType h (liftFromDotList t a) state

            Just name ->
                return $ addVariableType name a state

handleReturn :: InternalState -> Maybe (Expr e) -> e -> DuckTest e InternalState
{-| Handle the observation of a return statement. THis will
 - tell the current state that a return was hit and the return
 - type was given. -}
handleReturn state expr' pos | returnHit state = return state
                             | otherwise =
                                let expr = fromMaybe (None pos) expr' in
                                flip setReturnType state <$>
                                    inferTypeForExpression state expr

handleFunction :: InternalState -> Statement SrcSpan -> DuckTest SrcSpan InternalState
{-| Handles a function observation `def <function>(...): ...'. When
 - this happens, there are two phases; we attempt to infer the type of the
 - parameters via type-observation, then we infer via type matching the
 - return type of the function -}
handleFunction state fun@Fun {fun_name = (Ident name _), fun_body=body} = do
    functionType <- inferTypeForFunction state fun
    let newstate = addVariableType name functionType state
    case functionType of
       (Functional args _) -> do
          ret <- getReturnType <$> runChecker (addAll args newstate) body
          let newfntype = Functional args ret
          Info %%! duckf "\n(Inferred) " name " :: " newfntype "\n"
          return $ addVariableType name newfntype state
       _ -> do
           Warn %% "This should not happen, infer type of function returned a type that isn't a function."
           return state
handleFunction state _ = return state

handleForLoop :: InternalState -> [Expr SrcSpan] -> Expr SrcSpan -> [Statement SrcSpan] -> [Statement SrcSpan] -> SrcSpan -> DuckTest SrcSpan InternalState
handleForLoop state targets generator body elsebody pos = do
    generatorVariableType <-
        inferTypeForExpression state
            -- <generator>.__iter__().__next__()
            (Call (Dot (Call (Dot generator (Ident "__iter__" pos) pos) [] pos) (Ident "__next__" pos) pos) [] pos)

    let newVariables = flip mapMaybe targets $ \target ->
                          case target of
                            (Var (Ident name _) _) ->
                                Just (name, generatorVariableType)
                            _ -> Nothing

    let forLoopInitState = addAll newVariables state
    afterForLoopState <- runChecker forLoopInitState body
    afterElseState <- runChecker state elsebody
    return $ intersectStates afterForLoopState afterElseState


handleClass :: InternalState -> String -> [Statement SrcSpan] -> SrcSpan -> DuckTest SrcSpan InternalState
{-| Handle observing a class.-}
handleClass state name body pos = do
    {- This is done in several parts thanks to Python's annoying
     - semantics when it comes to dealing with classes. First stage,
     - we infer the *static* varibales of the class. Then we can
     - go through and check all the methods as if they are static methods.
     -
     - This will give us the static type of the class. That means
     - the structural type representing the class object itself. This
     - is contrasted with the instance type.
     -
     - We then build the instance type by observing all self assingments
     - in the __init__ function and the functions of the static
     - type less the self parameter. Finally, we ad a __call__ method
     - to the class with the same parameters as __init__ but returns
     - an instance type.... classes are a bitch to deal with.-}
    staticVarsState <- foldM' mempty body $ \curstate stmt ->
        case stmt of
            (Assign [Var (Ident vname _) _] ex _) -> do
                 inferredType <- inferTypeForExpression curstate ex
                 return $ addVariableType vname inferredType curstate
            _ -> return curstate

    let staticVarType = stateToType staticVarsState `union` Functional [] (Alpha Void)
    let newstate = addVariableType name staticVarType state
    staticClassState <- runChecker newstate $ mapMaybe (\stmt ->
                          case stmt of
                            Fun {} -> Just stmt
                            _ -> Nothing) body
    let staticClassType@Scalar {} = staticVarType `union` stateToType (differenceStates staticClassState newstate)
    let nextstate = addVariableType name staticClassType state

    boundType <- rewireAlphas <$>
                 toBoundType name staticClassType <$>
                 findSelfAssignments nextstate body

    Info %%! duckf "Bound type " boundType

    let classFunctionalType = initType boundType
    let staticClassType' = rewireAlphas' boundType (staticClassType `union` classFunctionalType)
    let laststate = addVariableType name staticClassType' state


    matchBoundWithStatic pos boundType staticClassType'

    return laststate

handleAssign :: InternalState -> String -> Expr a -> a -> DuckTest a InternalState
{-| Handle an assignment. a = <expr>. This will extend the state
 - to include the variable a with the type inferred from expr. If
 - the type happens to be inferred to be a void type, then a warning
 - is emitted warning of the void type usage. -}
handleAssign state vname ex pos = do
    inferredType <- inferTypeForExpression state ex

    when (isVoid2 inferredType) $
        warn pos $ duckf "Void type not ignored as it ought to be!"

    return $ addVariableType vname inferredType state

handleConditional :: InternalState -> [(Expr SrcSpan, [Statement SrcSpan])] -> [Statement SrcSpan] -> DuckTest SrcSpan InternalState
{-| Handle a conditional. This will run a checker on all the different branches
    and at the end, intersect all the states before continuing. This includes
    intersecting the types as well. -}
handleConditional state guards elsebody = do
    endStates <- forM guards $ \(expr, stmts) -> do
                  let modifiedState = case expr of
                                            (Call (Var (Ident "hasattr" _) _) [ArgExpr (Var (Ident x _) _) _, ArgExpr (Call (Var (Ident "str" _) _) [ArgExpr (Strings s _) _] _) _] _)
                                                ->  let s' = takeWhile (/='"') (tail $ concat s) in
                                                    modifyVariableType x (`union` singleton s' Any) state
                                            (Call (Dot (Dot (Var (Ident var _) _)
                                                  (Ident "__class__" _) _)
                                                  (Ident "__eq__" _) _)
                                                  [ArgExpr (Var (Ident clazz _) _) _] _)
                                                ->
                                                    let instanceType = instanceTypeFromStatic =<< getVariableType state clazz in
                                                        maybe state (\t -> modifyVariableType var (const t) state) instanceType
                                            _ -> state
                  _ <- inferTypeForExpression state expr
                  runChecker modifiedState stmts

    elseState <- runChecker state elsebody
    return $ foldl intersectStates elseState endStates

handleAttributeAssign :: InternalState -> Expr a -> String -> Expr a -> DuckTest a InternalState
handleAttributeAssign state lhs att rhs = do
    rhsType <- inferTypeForExpression state rhs
    _ <- inferTypeForExpression state lhs

    let typeToAssign = singleton att rhsType
    return $ assignType lhs typeToAssign

    where

    assignType (Var (Ident var _) _) typ = modifyVariableType var (`union` typ) state
    assignType (Dot expr (Ident attr _) _) typ = assignType expr $ singleton attr typ
    assignType _ _ = state


instance CheckerState InternalState where
    foldFunction currentState statement = do
        Trace %%! duckf "check: " statement

        when (returnHit currentState) $
            warn (annot statement) $ duckf "Dead code"

        case statement of

            (Import [ImportItem dotted@(_:_) as pos] _) ->
                let (h:t) = map (\(Ident name _) -> name) dotted in
                handleImport currentState (h, t) (getIdentifier =<< as) pos

            (Return expr pos) ->
                handleReturn currentState expr pos

            Fun {} ->
                handleFunction currentState statement

            (Class (Ident name _) [] body pos) ->
                handleClass currentState name body pos


            Assign [Var (Ident vname _) _] ex pos ->
                handleAssign currentState vname ex pos

            Assign [(Dot lhs (Ident att _) _)] rhs _ ->
                handleAttributeAssign currentState lhs att rhs

            Conditional {cond_guards=guards, cond_else=elsebody} ->
                handleConditional currentState guards elsebody

            (For targets generators body elsebody pos) ->
                handleForLoop currentState targets generators body elsebody pos

            _ -> do
                 mapM_ (inferTypeForExpression currentState) (subExpressions statement)
                 return currentState
