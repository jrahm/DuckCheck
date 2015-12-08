module DuckTest.Infer.Expression where

import DuckTest.Types
import DuckTest.Internal.Common
import DuckTest.Internal.State
import DuckTest.Monad
import DuckTest.MonadHelper
import DuckTest.Internal.Format

inferTypeForExpressionNoStrip :: InternalState -> Expr e -> DuckTest e PyType
inferTypeForExpressionNoStrip state expr =
    case expr of

      (Var (Ident name pos) _) ->
          maybe' (getVariableType state name)
              (warn pos (duckf "The identifier " name " may not be defined") >> return Any)
              return

      (Call callexpr args pos) -> do
          Debug %%! duckf Yellow "Infer call expression: " Blue expr Reset
          exprType <- checkCallExpression state callexpr args pos
          case getCallType exprType of
              Just (Functional _ ret) -> do
                Info %%! duckf "The type of " callexpr " returns type " ret
                return ret
              _ -> return Any

      (Dot subexpr (Ident att _) pos) -> do
          subexprType <- inferTypeForExpression state subexpr
          Trace %% "Infer for type | " ++ prettyText subexpr ++ " :: " ++ prettyType subexprType
          case subexprType of
              Any -> return Any
              _ -> case getAttribute att subexprType of
                      Nothing -> do
                          warn pos $ duckf "The subexpression " subexpr " may have no attribute '" att "' (" subexpr " :: " subexprType ")"
                          return Any
                      Just t -> return t
      (Paren subexpr _) -> inferTypeForExpression state subexpr
      (None _) -> return Any
      _ -> return Any

inferTypeForExpression :: InternalState -> Expr e -> DuckTest e PyType
inferTypeForExpression state expr = do
    Trace %%! duckf "TEST => " expr
    inferTypeForExpressionNoStrip state expr


checkCallExpression :: InternalState -> Expr e -> [Argument e] -> e -> DuckTest e PyType
checkCallExpression st lhs args pos = do
    lhsType <- inferTypeForExpression st lhs
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
            argType <- inferArgType state argExpr
            whenJust (matchType paramType argType)
                (warnTypeError position)

inferArgType :: InternalState -> Argument e -> DuckTest e PyType
inferArgType st (ArgExpr expr _) = inferTypeForExpression st expr
inferArgType _ _ = return Any
