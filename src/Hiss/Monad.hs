module Hiss.Monad (Hiss, runHiss) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans (lift)

import Control.Monad (when)

import Hiss.Flags
import Data.Set (Set)

data HissState = HissState {
    flags :: Set Flag
}

type Hiss = StateT HissState IO

runHiss :: Set Flag -> Hiss a -> IO a
runHiss flags fn = evalStateT fn (HissState flags)

hlog :: String -> Hiss ()
hlog str = lift $ putStrLn str

verbose :: String -> Hiss ()
verbose str = do
    isVerbose <- (Verbose `elem`) . flags <$> get
    when isVerbose $ hlog str
