module DuckTest.Internal.State where

import DuckTest.Internal.Common
import DuckTest.Types

import qualified Data.Map as Map

{- An internal checker state with a map of variables to their types
 - and a possible return type. -}
data InternalState = InternalState (Map String PyType) PyType Bool deriving Show

instance Monoid InternalState where
    mempty = InternalState mempty Any False
    mappend (InternalState m1 ret1 b1) (InternalState m2 ret2 b2) =
            InternalState (mappend m1 m2) (ret1 >< ret2) (b1 && b2)

differenceStates :: InternalState -> InternalState -> InternalState
differenceStates (InternalState m a b) (InternalState m1 _ _) =
    InternalState (Map.difference m m1) a b

stateToType :: InternalState -> PyType
stateToType (InternalState m _ _) = Scalar Nothing m

intersectStates :: InternalState -> InternalState -> InternalState
intersectStates (InternalState m1 r1 b1) (InternalState m2 r2 b2) =
    InternalState (Map.intersectionWith (><) m1 m2) (r1 >< r2) (b1 && b2)

getFunctionType :: InternalState -> String -> Maybe ([PyType], PyType)
getFunctionType st id' =
    (>>=) (getVariableType st id') $ \t -> case t of
        (Functional a b) -> return (map snd a, b)
        _ -> Nothing

getVariableType :: InternalState -> String -> Maybe PyType
getVariableType (InternalState map' _ _) str = Map.lookup str map'

addVariableType :: String -> PyType -> InternalState -> InternalState
addVariableType str typ (InternalState m1 r b) = InternalState (Map.insert str typ m1) r b

addAll :: [(String, PyType)] -> InternalState -> InternalState
addAll lst init' = foldl (\st (str, typ) -> addVariableType str typ st) init' lst

hasVariable :: String -> InternalState -> Bool
hasVariable vid = isJust . flip getVariableType vid

emptyState :: InternalState
emptyState = mempty

stateUnderFunction :: PyType -> InternalState -> InternalState
stateUnderFunction (Functional args _) = addAll args
stateUnderFunction _ = id

setReturnType :: PyType -> InternalState -> InternalState
setReturnType t (InternalState m _ _) = InternalState m t True

returnHit :: InternalState -> Bool
returnHit (InternalState _ _ True) = True
returnHit _ = False

modifyVariableType :: String -> (PyType -> PyType) -> InternalState -> InternalState
modifyVariableType var fn (InternalState m a b) =
                    InternalState (Map.update (Just . fn) var m) a b

getReturnType :: InternalState -> PyType
getReturnType (InternalState _ _ False) = Void
getReturnType (InternalState _ ret _) = ret
