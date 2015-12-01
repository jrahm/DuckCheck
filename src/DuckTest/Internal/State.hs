module DuckTest.Internal.State where

import DuckTest.Internal.Common
import DuckTest.Types
import DuckTest.Monad
import DuckTest.Parse

import qualified Data.Map as Map

newtype InternalState = InternalState (Map String PyType) deriving Show

instance Monoid InternalState where
    mempty = InternalState mempty
    mappend (InternalState m1) (InternalState m2) = InternalState (mappend m1 m2)

stateToType :: InternalState -> PyType
stateToType (InternalState m) = Scalar $ Attributes Nothing m

intersectStates :: InternalState -> InternalState -> InternalState
intersectStates (InternalState m1) (InternalState m2) = InternalState (Map.intersection m1 m2)

getFunctionType :: InternalState -> String -> Maybe ([PyType], PyType)
getFunctionType st id =
    (>>=) (getVariableType st id) $ \t -> case t of
        (Functional a b) -> return (map snd a, b)
        _ -> Nothing

getVariableType :: InternalState -> String -> Maybe PyType
getVariableType (InternalState map') str = Map.lookup str map'

addVariableType :: String -> PyType -> InternalState -> InternalState
addVariableType str typ (InternalState m1) = InternalState (Map.insert str typ m1)

addAll :: [(String, PyType)] -> InternalState -> InternalState
addAll lst init = foldl (\st (str, typ) -> addVariableType str typ st) init lst

hasVariable :: String -> InternalState -> Bool
hasVariable vid = isJust . flip getVariableType vid

emptyState :: InternalState
emptyState = InternalState mempty

stateUnderFunction :: PyType -> InternalState -> InternalState
stateUnderFunction (Functional args _) = addAll args
stateUnderFunction _ = id
