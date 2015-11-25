{-# LANGUAGE RankNTypes #-}
module DuckTest.AST.Util where

import DuckTest.Internal.Common

import Control.Monad.Writer.Lazy
import Control.Monad.Identity
import Debug.Trace

class HasExpressions a where

    {- Get all the sub expressions from a type `a'.
     - This will not recurse to get the sub expressions
     - from all levels, but rather just the first level
     - expressions -}
    subExpressions :: a e -> [Expr e]

    mapExpressionsM :: (Monad m) => (Expr e -> m (Expr e)) -> a e -> m (a e)

mapExpressions :: (HasExpressions a) => (Expr e -> Expr e) -> a e -> a e
mapExpressions fn expr = runIdentity $ mapExpressionsM (return . fn) expr

{- Walk through the expressions in a list of
 - has-expressions. -}
allExpressions :: (HasExpressions expr) => [expr a] -> [Expr a]
allExpressions = concatMap subExpressions

newtype ExprList a e = ExprList [a e]
data Command a = Recurse | Skip | Only [Expr a]

dotToList :: Expr a -> [String]

dotToList (Dot (Var (Ident name _) _) (Ident attribute _) _) =
    [name, attribute]

dotToList (Dot expr (Ident attr _) _) =
    if null (dotToList expr) then []
        else dotToList expr ++ [attr]

dotToList _ = []

{- Fold a value over a list of expressions -}
walkExpressions :: (HasExpressions expr) => a -> expr e -> (a -> Expr e -> (a, Command e)) -> a
walkExpressions init expression fn = walkExpressions' init (subExpressions expression)
    where walkExpressions' =
            foldl $ \cur expr ->
                    let (next, cmd) = fn cur expr in
                        case cmd of
                            Recurse -> walkExpressions' next (subExpressions expr)
                            Skip -> next
                            Only exps -> walkExpressions' next exps

{- Walks through the expressions in an has-expression using a monad. -}
walkExpressionM :: (HasExpressions expr, Monad m) => expr e -> (Expr e -> m (Command e)) -> m ()
walkExpressionM expr = walkExpressions' (subExpressions expr)
    where
        walkExpressions' :: (Monad m) => [Expr e] -> (Expr e -> m (Command e)) -> m ()
        walkExpressions' exprs fn =
            forM_ exprs $ \exp -> do
                cmd <- fn exp
                case cmd of
                    Recurse -> walkExpressions' (subExpressions exp) fn
                    Skip -> return ()
                    Only exps -> walkExpressions' exps fn


{- Walk through all expressions in the given list of has-expressions
 - in pre-order format. -}
recursivelyWalkExpressions :: (HasExpressions expr) => [expr a] -> [Expr a]
recursivelyWalkExpressions stmt =
    execWriter $ forM_ (allExpressions stmt) walk
    where
        walk :: Expr a -> Writer [Expr a] ()
        walk exp = tell [exp] >> mapM_ walk (subExpressions exp)

instance HasExpressions Expr where

    subExpressions exp =

        case exp of
            Call fn args _ -> fn : map getExpression args
            Subscript e1 e2 _ -> [e1, e2]
            SlicedExpr e1 _ _ -> return e1 -- todo implement strides
            CondExpr e1 e2 e3 _ -> [e1, e2, e3]
            BinaryOp _ e1 e2 _ -> [e1, e2]
            UnaryOp _ e1 _ -> return e1
            Lambda _ body _ -> return body
            Tuple exprs _ -> exprs
            Yield (Just (YieldFrom expr _)) _ -> return expr
            Yield (Just (YieldExpr expr)) _ -> return expr
            Yield Nothing _ -> []
            Generator _ _ -> [] -- todo implement these
            ListComp _ _ -> []
            DictComp _ _ -> []
            SetComp _ _ -> []
            Dictionary mappings _ -> concatMap (\(DictMappingPair e1 e2) -> [e1, e2]) mappings
            Set exprs _ -> exprs
            Starred exp _ -> return exp
            Paren exp _ -> return exp
            StringConversion exp _ -> return exp
            Dot exp _ _ -> return exp
            List exps _ -> exps
            _ -> []

    mapExpressionsM f exp =

        case exp of
            Call fn args pos -> Call <$> f fn <*> mapM (mapExpressionsM f) args <*> pure pos
            Subscript e1 e2 pos -> Subscript <$> f e1 <*> f e2 <*> pure pos
            CondExpr e1 e2 e3 pos -> CondExpr <$> f e1 <*> f e2 <*> f e3 <*> pure pos
            BinaryOp nam e1 e2 pos -> BinaryOp nam <$> f e1 <*> f e2 <*> pure pos
            UnaryOp name e1 pos -> UnaryOp name <$> f e1 <*> pure pos
            Lambda param body pos -> Lambda param <$> f body <*> pure pos
            Tuple exprs pos -> Tuple <$> (mapM f exprs) <*> pure pos
            Yield (Just (YieldFrom expr p)) pos -> (\e -> Yield (Just (YieldFrom e p)) pos) <$> f expr
            Yield (Just (YieldExpr expr)) pos -> (\e -> Yield (Just (YieldExpr e)) pos) <$> f expr

            Dictionary mappings pos -> Dictionary <$>
                    mapM (\(DictMappingPair e1 e2) -> DictMappingPair <$> f e1 <*> f e2) mappings <*>
                    pure pos

            Set exprs pos -> Set <$> mapM f exprs <*> pure pos
            Starred exp pos -> Starred <$> f exp <*> pure pos
            Paren exp pos -> Paren <$> f exp <*> pure pos
            StringConversion exp pos -> StringConversion <$> f exp <*> pure pos
            Dot exp att pos -> Dot <$> f exp <*> pure att <*> pure pos
            List exps pos -> List <$> mapM f exps <*> pure pos

            _ -> return exp {- This is some unimplemented stuff ... -}

instance HasExpressions Argument where

    subExpressions (ArgExpr e _) = [e]
    subExpressions (ArgVarArgsPos e _) = [e]
    subExpressions (ArgVarArgsKeyword e _) = [e]
    subExpressions (ArgKeyword _ e _) = [e]

    mapExpressionsM f (ArgExpr e pos) = ArgExpr <$> f e <*> pure pos
    mapExpressionsM f (ArgVarArgsPos e pos) = ArgVarArgsPos <$> f e <*> pure pos
    mapExpressionsM f (ArgVarArgsKeyword e pos) = ArgVarArgsKeyword <$> f e <*> pure pos
    mapExpressionsM f (ArgKeyword str e pos) = ArgKeyword str <$> f e <*> pure pos

instance HasExpressions Statement where

    subExpressions stmt =
        case stmt of
            While condition body elsestmt _ ->
                condition : (allExpressions body ++
                             allExpressions elsestmt)

            For targets generator body elsestmt _ ->
                targets ++ [generator] ++ allExpressions body
                                       ++ allExpressions elsestmt

            Fun _ _ res body _ ->
                maybeToList res ++ allExpressions body

            Class _ _ body _ ->
                allExpressions body

            Conditional guards elsestmt _ ->
                concatMap (\(ex, suite) -> ex : allExpressions suite) guards ++
                allExpressions elsestmt

            Assign to from _ -> to ++ [from]

            AugmentedAssign to _ from _ -> [to, from]

            Return exp _ -> maybeToList exp

            Try body handlers elses finally _ ->
                allExpressions body ++ concatMap (\(Handler _ body _) -> allExpressions body) handlers ++
                allExpressions elses ++ allExpressions finally

            Raise exp _ -> [] -- todo implement this
            With context body _ ->
                concatMap (\(ex1, mayex2) -> ex1 : maybeToList mayex2) context ++ allExpressions body

            Delete exp _ -> exp
            StmtExpr exp _ -> return exp

            Assert exp _ -> exp
            Print _ exp _ _ -> exp

            Exec exp _ _ -> return exp -- todo implement

            _ -> []

    mapExpressionsM f stmt =
        case stmt of
            While condition body elsestmt pos ->
                While <$> f condition <*>
                mapM (mapExpressionsM f) body <*>
                mapM (mapExpressionsM f) elsestmt <*> pure pos

            For targets generator body elsestmt pos ->
                For <$> pure targets
                    <*> f generator
                    <*> mapM (mapExpressionsM f) body
                    <*> mapM (mapExpressionsM f) elsestmt
                    <*> pure pos

            Fun a b res body pos ->
                Fun a b <$> mapM (mapExpressionsM f) res
                        <*> mapM (mapExpressionsM f) body
                        <*> pure pos

            Class a b body pos ->
                Class a b <$> mapM (mapExpressionsM f) body
                          <*> pure pos

            Conditional guard elsestmt pos ->
                Conditional <$> mapM (\(ex, suite) ->
                                    (,) <$> f ex <*> mapM (mapExpressionsM f) suite) guard
                            <*> mapM (mapExpressionsM f) elsestmt
                            <*> pure pos

            Assign to from pos ->
                Assign <$> mapM f to
                       <*> f from
                       <*> pure pos

            AugmentedAssign to op from pos ->
                AugmentedAssign <$> f to
                                <*> pure op
                                <*> f from
                                <*> pure pos

            Return exp pos -> Return <$> mapM f exp <*> pure pos

            -- Try body handlers elses finally _ ->
            --     allExpressions body ++ concatMap (\(Handler _ body _) -> allExpressions body) handlers ++
            --     allExpressions elses ++ allExpressions finally

            -- Raise exp _ -> [] -- todo implement this
            -- With context body _ ->
            --     concatMap (\(ex1, mayex2) -> ex1 : maybeToList mayex2) context ++ allExpressions body

            -- Delete exp _ -> exp
            StmtExpr exp pos -> StmtExpr <$> f exp <*> pure pos

            -- Assert exp _ -> exp
            -- Print _ exp _ _ -> exp

            -- Exec exp _ _ -> return exp -- todo implement

            exp -> return exp -- unhandled stuff

{- Iterate through all sub-statements -}
walkStatements :: Statement a -> [Statement a]
walkStatements stmt =
    case stmt of
        (Fun {fun_body = body}) -> stmt : concatMap walkStatements body
        (Class {class_body = body}) -> stmt : concatMap walkStatements body
        (While {while_body = body}) -> stmt : concatMap walkStatements body
        (For {for_body = body, for_else = el}) -> stmt : concatMap walkStatements body ++ concatMap walkStatements el
        _ -> return stmt -- todo add more

class HasIdentifier a where
     getIdentifier :: a -> Maybe String

class HasExpression a where
    getExpression :: a e -> Expr e

instance HasExpression Argument where
    getExpression (ArgExpr e _) = e
    getExpression (ArgVarArgsPos e _) = e
    getExpression (ArgVarArgsKeyword e _) = e
    getExpression (ArgKeyword _ e _) = e

instance (HasExpressions e) => HasExpressions (ExprList e) where
    subExpressions (ExprList a) = concatMap subExpressions a


instance HasIdentifier (Parameter a) where
    getIdentifier s = case s of
            Param (Ident n _) _ _ _ -> Just n
            VarArgsPos (Ident n _) _ _ -> Just n
            VarArgsKeyword (Ident n _) _ _ -> Just n
            _ -> Nothing

tryGetIdentifier :: (HasIdentifier a) => String -> a -> String
tryGetIdentifier str = fromMaybe str . getIdentifier
