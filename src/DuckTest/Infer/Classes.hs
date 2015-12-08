{-# LANGUAGE ScopedTypeVariables #-}
module DuckTest.Infer.Classes where

import DuckTest.Internal.Common hiding (union)

import DuckTest.Monad
import DuckTest.MonadHelper
import DuckTest.Types

import qualified Data.Map as Map

import DuckTest.Infer.Expression
import DuckTest.Internal.State
import DuckTest.Internal.Format
import Control.Arrow


{- Takes a state and a list of statements (the class body) and finds
 - the self assignments. This returns a PyType as a scalar of all
 - the found assignments.
 -
 - Essentially, this function traces the __init__ function and
 - all functions called directly from it and looks for expressions
 - like self.x = expr. It then knows that if expr :: t then
 - self :: {x :: t} u tau (where tau is the rest of the type of self)
 -}
findSelfAssignments :: forall a. InternalState -> [Statement a] -> DuckTest a PyType
findSelfAssignments state statements =
    foldM traceAssignments Void $ functionList statements
    where
        functionList :: [Statement a] -> [[Statement a]]
        functionList = mapMaybe $ \stmt ->
                        case stmt of
                            Fun {fun_body = body} -> Just body
                            _ -> Nothing

        traceAssignments :: PyType -> [Statement a] -> DuckTest a PyType
        traceAssignments ini stmts =
            foldM' ini stmts  $ \typ stmt -> case stmt of

                (Assign [Dot (Var (Ident "self" _) _) (Ident att _) _] ex _) -> do
                    inferred <- inferTypeForExpressionNoStrip (addVariableType "self" typ state) ex
                    return (tryIntersect typ att inferred)

                _ -> return typ

        tryIntersect :: PyType -> String -> PyType -> PyType
        tryIntersect typ attname atttype =
            let curattr = fromMaybe Any (getAttribute attname typ)
                toadd = case curattr of
                            Any -> atttype
                            t1 -> t1 `intersection` atttype
                in
                setAttribute attname toadd typ

{- From a PyType, get the init function from it. If we cannot get the
 - init function from it, we simply create an itit function type. -}
initType :: PyType -> PyType
initType f@(Scalar _ m) =
    let initFn = Map.findWithDefault (Functional [] f) "__init__" m in
    case initFn of
        (Functional args _) -> Functional args f
        _ -> Functional [] f
initType f = Functional [] f

{- Given a instance type of a class, check to make sure
 - it is consistent with the types expected for `self`
 - in all other functions. -}
matchBoundWithStatic :: e -> PyType -> PyType -> DuckTest e ()
matchBoundWithStatic pos bound (Scalar _ m) =
    forM_ (map snd $ Map.toList m) $ \typ ->
        case typ of
            (Functional ((_, self):_) _) ->
                whenJust (matchType self bound)
                    (warnTypeError pos)
            _ -> return ()
matchBoundWithStatic _ _ _ = undefined

{- Given self assignments (found from the above function) and the
 - sattic type of the class, create a bound type instance that
 - has the same functions as the static instance, but with the
 - `self' parameter removed.
 -}
toBoundType :: String -> PyType -> PyType -> PyType
toBoundType name (Scalar _ m2) selfAssign =
    let boundFns = Scalar (Just name) $
                    flip Map.mapMaybe m2 $
                        \typ -> case typ of
                                 (Functional (_:as) r) -> Just (Functional as r)
                                 _ -> Nothing
        in
    setTypeName name $ selfAssign `union` boundFns
toBoundType _ _ _ = undefined

rewireAlphas :: PyType -> PyType
{-| finds all Alpha Any's in a type and changes
 - them to Alpha t's. -}
rewireAlphas typ = rewireAlphas' typ typ

rewireAlphas' :: PyType -> PyType -> PyType
rewireAlphas' top (Alpha Void) = mkAlpha (rewireAlphas' top top)
rewireAlphas' top (Alpha t) = mkAlpha (rewireAlphas' top t)
rewireAlphas' top (Functional args ret) = Functional (map (second $ rewireAlphas' top) args) (rewireAlphas' top ret)
rewireAlphas' top (Scalar s m) = Scalar s (Map.map (rewireAlphas' top) m)
rewireAlphas' _ Any = Any
rewireAlphas' _ Void = Void
