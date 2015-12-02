module DuckTest.Internal.State where

import DuckTest.Internal.Common
import DuckTest.Types

import qualified Data.Map as Map

{- An internal checker state with a map of variables to their types
 - and a possible return type. -}
data InternalState = InternalState (Map String PyType) (Maybe PyType) Bool deriving Show

instance Monoid InternalState where
    mempty = InternalState mempty Nothing False
    mappend (InternalState m1 ret1 b1) (InternalState m2 ret2 b2) = InternalState (mappend m1 m2) (mappend ret1 ret2) (b1 && b2)

stateToType :: InternalState -> PyType
stateToType (InternalState m _ _) = Scalar $ Attributes Nothing m

intersectStates :: InternalState -> InternalState -> InternalState
intersectStates (InternalState m1 r1 b1) (InternalState m2 r2 b2) =
    InternalState (Map.intersection m1 m2) (inter r1 r2) (b1 && b2)
    where inter Nothing x = x
          inter x Nothing = x
          inter (Just x) (Just y) = Just $ x >< y

getFunctionType :: InternalState -> String -> Maybe ([PyType], PyType)
getFunctionType st id =
    (>>=) (getVariableType st id) $ \t -> case t of
        (Functional a b) -> return (map snd a, b)
        _ -> Nothing

getVariableType :: InternalState -> String -> Maybe PyType
getVariableType (InternalState map' _ _) str = Map.lookup str map'

addVariableType :: String -> PyType -> InternalState -> InternalState
addVariableType str typ (InternalState m1 r b) = InternalState (Map.insert str typ m1) r b

addAll :: [(String, PyType)] -> InternalState -> InternalState
addAll lst init = foldl (\st (str, typ) -> addVariableType str typ st) init lst

hasVariable :: String -> InternalState -> Bool
hasVariable vid = isJust . flip getVariableType vid

emptyState :: InternalState
emptyState = InternalState mempty Nothing False

stateUnderFunction :: PyType -> InternalState -> InternalState
stateUnderFunction (Functional args _) = addAll args
stateUnderFunction _ = id

setReturnType :: PyType -> InternalState -> InternalState
setReturnType t (InternalState m _ _) = InternalState m (Just t) True

returnHit :: InternalState -> Bool
returnHit (InternalState _ _ True) = True
returnHit _ = False
