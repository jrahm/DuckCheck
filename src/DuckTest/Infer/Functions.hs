{-# LANGUAGE RankNTypes, TupleSections #-}

{- This module is dedicated in infering the type of functions.
 - The type of a function consists of two separate parts:
 -
 - 1. The type of the arguments.
 - 2. The type of the return type.
 -
 - Currently, DuckTest cannot infer the return type,
 - however, DuckTest is able to infer the type of the arguments.
 -
 - DuckTest infers the type by looking at what other functions
 - the argument is passed to as well as observing the
 - attributes used by the function on that argument
 -}

module DuckTest.Infer.Functions (inferTypeForFunction) where

import DuckTest.Internal.Common

import DuckTest.Monad
import DuckTest.AST.Util
import DuckTest.Types
import DuckTest.Internal.State
import DuckTest.Internal.Format
import DuckTest.Infer.Expression


{- This function will take a Python function and infer the type
 - of this function. The type infered from this function is
 - of the type [args] -> return type. All the types are in a
 - structural format -}
inferTypeForFunction :: InternalState -> Statement a -> DuckTest a PyType
inferTypeForFunction state (Fun (Ident name _) params _ body _) =

    {- Get a list of the names of the parameters to the function. For
     - each of those parameters, try to infer the type of each.
     -
     - Return type is not yet able to be inferred. This is a TODO -}
    let parameterIdentifiers :: [String]
        parameterIdentifiers = map (tryGetIdentifier "") params

        returnType = Any -- cannot yet infer return type

        newstate = addVariableType name (Functional (map (const ("",Any)) params) Any) state
        in

        flip Functional returnType <$>
                (zip parameterIdentifiers <$>
                 mapM (flip (inferTypeForVariable newstate) body) parameterIdentifiers)


inferTypeForFunction _ _ =
    {- This function was called on something not a function -}
    die "inferTypeForFunction called on non function!"

{- Collects and infers the type of a variable name over
 - the span of the list of statements given. -}
inferTypeForVariable :: forall e. InternalState -> String -> [Statement e] -> DuckTest e PyType
inferTypeForVariable state varname = observeTypeForExpression state (Var (Ident varname ()) ())

observeTypeForExpression :: InternalState -> Expr b -> [Statement e] -> DuckTest e PyType
observeTypeForExpression state expr stmts = do
        {- This function, we walk through each expression in the
         - body of statements. In each expression, we look to see
         - how the parameter is being used. -}
            fn <- unwrap <$> mconcatMapM (Union <.< observeExpr) (allExpressions stmts)
            Debug %%! duckf "Observe " expr " :: " fn
            return fn

        where
        {- An observation of the pattern `x.y` we use this
         - to infer that the argument `x` must have an attribute
         - `y` -}
        observeExpr ex@(Dot e (Ident attname _) _)
                     | e `exprEq` expr = do
                        {- TODO infer the expression -}
                        typ <- observeTypeForExpression state ex stmts
                        return $ singleton attname typ

        observeExpr ex@(Call e args _)
                    | e `exprEq` expr =
                        Functional (map (const ("", Void)) args) <$>
                            observeTypeForExpression state ex stmts

        {- An observation where we call a function with x as an argument.
         - We use this to retrieve more information about `x`. Specifically,
         - look up what `x` requires for this function to be valid.
         -
         - This observation is of function(..., x, ...)-}
        observeExpr outerexpr@(Call callex args _) = do
                 calltyp <- ignore $ inferTypeForExpression state callex
                 maybe' (getCallType calltyp) (iterateOverChildren outerexpr) $
                    \typ -> case typ of
                        (Functional paramsType  _) -> do

                            let inferTypeFromArguments (expr', exprType) =
                                    {- Iterate through the arguments to the observed
                                     - function call. If the name alone is observed,
                                     - then use infer the type for the argument, otherwise
                                     - search for evidence in the expression -}
                                    case getExpression expr' of

                                        ex | ex `exprEq` expr  ->
                                            trace ("typ " ++ prettyType exprType) $
                                            return exprType


                                        ex -> observeExpr ex

                            unwrap <$> mconcatMapM (Union <.< inferTypeFromArguments) (zip args $ map snd paramsType)

                        _ -> return Void

        observeExpr expr' = iterateOverChildren expr'


        {- infer through the child expressions of this expression -}
        iterateOverChildren expr' = unwrap <$> mconcatMapM (Union <.< observeExpr) (subExpressions expr')
