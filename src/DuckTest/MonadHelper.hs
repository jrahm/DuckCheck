{- This module is dedicated to untility function to make
 - using the DuckTest monad easier and less verbose-}

module DuckTest.MonadHelper where

import DuckTest.Monad
import Text.Printf

{- Emit a potentially undefined variable warning-}
possibleUndefinedError :: String -> a -> DuckTest a ()
possibleUndefinedError = emitWarning . printf "Possible undefined variable %s"

{- Emit a potential attribute error -}
possibleAttributeError :: String -> String -> a -> DuckTest a ()
possibleAttributeError ident = emitWarning . printf "Possible attribute error: %s has no attributes %s" ident
