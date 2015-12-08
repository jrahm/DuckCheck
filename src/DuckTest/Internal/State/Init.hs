{-# LANGUAGE TupleSections #-}
module DuckTest.Internal.State.Init where

import DuckTest.Builtins
import DuckTest.Internal.State
import DuckTest.Types

initState :: InternalState e
initState =
    addVariableType "hasattr" (Functional [("", Any), ("", strType)] Any) $
    addVariableType "print" (Functional [("", Any)] Void) $
    addVariableType "len" (Functional [("", fromList Nothing $ map (,Any) ["__len__"])] Any) $
    addVariableType "str" (Functional [("", fromList Nothing $ map (,Any) ["__str__"])] strType) $
    addVariableType "int" (Functional [("", Any)] intType) $
    addVariableType "set" (Functional [] setType) $
    addVariableType "list" (Functional [] $ listType Any)
    emptyState
