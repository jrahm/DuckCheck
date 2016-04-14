module DuckTest.Infer.Expression where

import DuckTest.Types
import DuckTest.Internal.Common
import DuckTest.Internal.State
import DuckTest.Monad
import DuckTest.MonadHelper
import DuckTest.Internal.Format

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

inferTypeForExpressionNoStrip :: InternalState -> Expr e -> DuckTest e PyType
inferTypeForExpressionNoStrip state expr =
    case expr of

      (Var (Ident name pos) _) ->
          maybe' (getVariableType state name)
              (warn pos (duckf "The identifier " name " may not be defined") >> return Any)
              return

      (Call callexpr args pos) -> do
          Debug %%! duckf Yellow "Infer call expression: " Blue expr Reset
          exprType <- inferTypeForExpression state callexpr
          case getCallType exprType of
              Just (Functional fn) -> do
                -- Info %%! duckf "The type of " callexpr " returns type " ret
                argTypes <- buildArgumentMap args
                retType <- liftIO (runEitherT $ fn pos argTypes)
                case retType of
                    Left (err, errpos) -> warnTypeError errpos err >> return Any
                    Right x -> return x
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
  where
    buildArgumentMap :: [Argument e] -> DuckTest e (Map String (PyType, e))
    buildArgumentMap = undefined

inferTypeForExpression :: InternalState -> Expr e -> DuckTest e PyType
inferTypeForExpression state expr = do
    Trace %%! duckf "TEST => " expr
    inferTypeForExpressionNoStrip state expr

inferArgType :: InternalState -> Argument e -> DuckTest e PyType
inferArgType st (ArgExpr expr _) = inferTypeForExpression st expr
inferArgType _ _ = return Any
