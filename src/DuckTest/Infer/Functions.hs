{-# LANGUAGE RankNTypes, TupleSections #-}

{- This module is dedicated in infering the type of functions.
 - The type of a function consists of two separate parts:
 -
 - 1. The type of the arguments.
 - 2. The type of the return type.
 -
 - Currently, DuckTest cannot infer the return type,
 - however, DuckTest is able to infer the type of the arguments.
 -
 - DuckTest infers the type by looking at what other functions
 - the argument is passed to as well as observing the
 - attributes used by the function on that argument
 -}

module DuckTest.Infer.Functions (inferTypeForFunction) where

import DuckTest.Internal.Common

import DuckTest.Monad
import DuckTest.AST.Util
import DuckTest.Types
import DuckTest.Internal.State
import DuckTest.Internal.Format
import DuckTest.Infer.Expression


{- This function will take a Python function and infer the type
 - of this function. The type infered from this function is
 - of the type [args] -> return type. All the types are in a
 - structural format -}
inferTypeForFunction :: InternalState -> Statement a -> DuckTest a PyType
inferTypeForFunction state (Fun (Ident name _) params _ body _) = undefined



inferTypeForFunction _ _ =
    {- This function was called on something not a function -}
    die "inferTypeForFunction called on non function!"
