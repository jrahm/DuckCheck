{-# LANGUAGE MultiParamTypeClasses #-}

{-| This mondule is all about checking the python syntax tree
    for potential problems. It really is at the heart of DuckTest
    as it is what is actually doing the checking -}

module DuckTest.Checker where

import DuckTest.Internal.Common hiding (union)
import DuckTest.Internal.Format
import DuckTest.Monad

class CheckerState s where
    {-| The function used in a monadic fold across a list of
        statements -}
    foldFunction :: s -> Statement SrcSpan -> DuckTest SrcSpan s

runChecker :: (CheckerState s) => s -> [Statement SrcSpan] -> DuckTest SrcSpan s
{-| Run a checker under some state s and return the resulting state
 - after the fold across the expressions. -}
runChecker initstate stmts = do
    Trace %%! duckf "Running checker on stmts"
    forM_ stmts $ \s -> Trace %%! duckf s
    foldM foldFunction initstate stmts

runChecker_ :: (CheckerState s) => s -> [Statement SrcSpan] -> DuckTest SrcSpan ()
{-| Like the above, but ignore the results -}
runChecker_ a = void . runChecker a
