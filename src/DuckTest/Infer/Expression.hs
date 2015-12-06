module DuckTest.Infer.Expression where

import DuckTest.Types
import DuckTest.Internal.Common
import DuckTest.Internal.State
import DuckTest.Monad
import DuckTest.MonadHelper
import DuckTest.Internal.Format

inferTypeForExpressionNoStrip :: InternalState e -> Expr e -> DuckTest e (Deferred e PyType)
inferTypeForExpressionNoStrip state expr =
    case expr of

      (Var (Ident name pos) _) ->
          maybe' (getVariableType state name)
              (warn pos (duckf "The identifier " name " may not be defined") >> return (pure Any))
              return

      (Call callexpr args pos) -> do
          exprType <- checkCallExpression state callexpr args pos
          case getCallType exprType of
              Just (Functional _ ret) -> do
                Trace %%! duckf "The type of " callexpr " returns type " ret
                return (pure ret)
              _ -> return (pure Any)

      (Dot subexpr (Ident att _) pos) -> do
          subexprType <- runDeferred state =<< inferTypeForExpression state subexpr
          Trace %% "Infer for type | " ++ prettyText subexpr ++ " :: " ++ prettyType subexprType
          case subexprType of
              Any -> return (pure Any)
              _ -> case getAttribute att subexprType of
                      Nothing -> do
                          warn pos $ duckf "The subexpression " subexpr " may have no attribute '" att "' (" subexpr " :: " subexprType ")"
                          return (pure Any)
                      Just t -> return (pure t)
      (Paren subexpr _) -> inferTypeForExpression state subexpr
      (None _) -> return (pure Any)
      _ -> return (pure Any)

inferTypeForExpression :: InternalState e -> Expr e -> DuckTest e (Deferred e PyType)
inferTypeForExpression state expr = do
    Trace %%! duckf "TEST => " expr
    inferTypeForExpressionNoStrip state expr


checkCallExpression :: InternalState e -> Expr e -> [Argument e] -> e -> DuckTest e PyType
checkCallExpression st lhs args pos = do
    lhsType <- runDeferred st =<< inferTypeForExpression st lhs
    case lhsType of
        Any -> return () -- maybe adda paranoid warning here
        _ -> case getAttribute "__call__" lhsType of
                Nothing -> warn pos $ duckf "The expression " lhs " does not appear to be callable"
                Just (Functional argTypes _) ->
                    zipWithM_ (tryMatchExprType st pos) (map snd argTypes) args
                Just t ->
                    warn pos $ duckf "The type for __call__ is not a function! Got " t
    return lhsType

    where
        tryMatchExprType state position paramType argExpr = do
            argType <- runDeferred st =<< inferArgType state argExpr
            whenJust (matchType argExpr paramType argType)
                (warnTypeError position)

inferArgType :: InternalState e -> Argument e -> DuckTest e (Deferred e PyType)
inferArgType st (ArgExpr expr _) = inferTypeForExpression st expr
inferArgType _ _ = return (pure Any)
