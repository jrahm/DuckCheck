{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module DuckTest.Internal.State where

import DuckTest.Internal.Common
import DuckTest.Types
import DuckTest.Monad

import qualified Data.Map as Map

{- An internal checker state with a map of variables to their types
 - and a possible return type. -}

data Deferred e t = Deferred (InternalState e -> DuckTest e t) | Calculated t
data InternalState e = InternalState (Map String (Deferred e PyType)) PyType Bool

deferredIntersection :: Deferred e PyType -> Deferred e PyType -> Deferred e PyType
deferredIntersection (Calculated t1) (Calculated t2) = Calculated (t1 >< t2)
deferredIntersection (Deferred f) (Calculated t2) =
                        Deferred $ \st -> (><) <$> f st <*> pure t2
deferredIntersection (Calculated t1) (Deferred f) =
                        Deferred $ \st -> (><) <$> pure t1 <*> f st
deferredIntersection (Deferred f1) (Deferred f2) = Deferred $ \st -> (><) <$> f1 st <*> f2 st

instance Functor (Deferred e) where
    fmap f' (Deferred f) = Deferred $ \st -> f' <$> (f st)
    fmap f' (Calculated t) = Calculated (f' t)

deferGet :: Deferred e t -> InternalState e -> DuckTest e t
deferGet (Deferred f) st = f st
deferGet (Calculated s) _ = return s

instance Applicative (Deferred e) where
    pure x = Deferred $ \_ -> return x

    (<*>) :: Deferred e (a -> b) -> Deferred e a -> Deferred e b
    (<*>) (Calculated f) (Calculated a)  =
            Calculated (f a)
    (<*>) f a = Deferred $ \st -> deferGet f st <*> deferGet a st

instance Monoid (InternalState e) where
    mempty = InternalState mempty Any False
    mappend (InternalState m1 ret1 b1) (InternalState m2 ret2 b2) =
            InternalState (mappend m1 m2) (ret1 >< ret2) (b1 && b2)

differenceStates :: InternalState e -> InternalState e -> InternalState e
differenceStates (InternalState m a b) (InternalState m1 _ _) =
    InternalState (Map.difference m m1) a b

runDeferred :: InternalState e -> Deferred e t -> DuckTest e t
runDeferred st (Deferred f) = f st
runDeferred _ (Calculated t) = return t

stateToType :: InternalState e -> DuckTest e PyType
stateToType st@(InternalState m _ _) = do
    let lst = Map.toList m
    newlst <- mapM (\(k, v) -> (,) k <$> runDeferred st v) lst
    return $ Scalar Nothing (Map.fromList newlst)

intersectStates :: InternalState e -> InternalState e -> InternalState e
intersectStates (InternalState m1 r1 b1) (InternalState m2 r2 b2) =
    InternalState (Map.intersectionWith deferredIntersection m1 m2) (r1 >< r2) (b1 && b2)

getFunctionType :: InternalState e ->
                   String ->
                   DuckTest e (Deferred e (Maybe ([PyType], PyType)))
getFunctionType st id' = do
    t <- getVariableType st id'
    return $
        case t of
            Just (Deferred f) -> Deferred $ \state -> getFunctionType' <$> (f state)
            Just (Calculated typ) -> Calculated $ getFunctionType' typ
            Nothing -> Calculated Nothing
    where getFunctionType' t =
            case t of
                (Functional a b) -> return (map snd a, b)
                _ -> Nothing

evalVariableType :: InternalState e -> String -> DuckTest e (Maybe PyType)
evalVariableType st str = do
    typ <- getVariableType st str
    case typ of
        Nothing -> return Nothing
        Just x -> do
            ret <- runDeferred st x
            return (Just ret)

getVariableType' :: InternalState e -> String -> Maybe (Deferred e PyType)
getVariableType' (InternalState map' _ _) str = Map.lookup str map'

getVariableType :: InternalState e -> String -> DuckTest e (Maybe (Deferred e PyType))
getVariableType st str = return $ getVariableType' st str

addVariableType :: String -> PyType -> InternalState e -> InternalState e
addVariableType str typ (InternalState m1 r b) = InternalState (Map.insert str (Calculated typ) m1) r b

addAll :: [(String, PyType)] -> InternalState e -> InternalState e
addAll lst init' = foldl (\st (str, typ) -> addVariableType str typ st) init' lst

addVariableTypeDeferred :: String -> Deferred e PyType -> InternalState e -> DuckTest e (InternalState e)
addVariableTypeDeferred str typ (InternalState m1 r b) = do
    typ' <- realType typ
    return $ InternalState (Map.insert str typ' m1) r b

    where realType (Deferred fn) = do
            duckRef <- newDuckRef Nothing
            return $ Deferred $ \st' -> do
                cache <- readDuckRef duckRef
                case cache of
                    Nothing -> do
                        t <- fn st'
                        writeDuckRef (Just t) duckRef
                        return t
                    Just t -> return t
          realType t = return $ t

addAllDeferred :: [(String, Deferred e PyType)] -> InternalState e -> DuckTest e (InternalState e)
addAllDeferred lst init' = foldM (\st (str, typ) -> addVariableTypeDeferred str typ st) init' lst

hasVariable :: String -> InternalState e -> Bool
hasVariable vid (InternalState m _ _) = Map.member vid m

emptyState :: InternalState e
emptyState = mempty

stateUnderFunction :: PyType -> InternalState e -> InternalState e
stateUnderFunction (Functional args _) = addAll args
stateUnderFunction _ = id

setReturnType :: PyType -> InternalState e -> InternalState e
setReturnType t (InternalState m _ _) = InternalState m t True

returnHit :: InternalState e -> Bool
returnHit (InternalState _ _ True) = True
returnHit _ = False

modifyVariableType :: String -> (PyType -> PyType) -> InternalState e -> InternalState e
modifyVariableType var fn (InternalState m a b) =
                    InternalState (Map.update (Just . fmap fn) var m) a b

getReturnType :: InternalState e -> PyType
getReturnType (InternalState _ _ False) = Void
getReturnType (InternalState _ ret _) = ret
