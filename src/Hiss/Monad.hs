module Hiss.Monad (Hiss, runHiss, hlog, verbose, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runHissIO) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either

import Control.Monad (when)

import Hiss.Flags
import Data.Set (Set)

import System.IO
import System.Exit (exitWith, ExitCode(ExitFailure))

data HissState = HissState {
    flags :: Set Flag
}

type Hiss = EitherT String (StateT HissState IO)

hissLiftIO :: IO a -> Hiss a
hissLiftIO = lift . lift

runHiss :: Set Flag -> Hiss a -> IO (Either String a)
runHiss flags fn = evalStateT (runEitherT fn) (HissState flags)

runHissIO :: Set Flag -> Hiss a -> IO a
runHissIO flags fn = do
    either <- evalStateT (runEitherT fn) (HissState flags)
    case either of
        Left s -> hPutStrLn stderr s >> exitWith (ExitFailure 1)
        Right a -> return a

hlog :: String -> Hiss ()
hlog str = lift $ lift $ putStrLn str

verbose :: String -> Hiss ()
verbose str = do
    isVerbose <- (Verbose `elem`) . flags <$> lift get
    when isVerbose $ hlog str

hasFlag :: Flag -> Hiss Bool
hasFlag f = (f `elem`) . flags <$> lift get

isVersion2 :: Hiss Bool
isVersion2 = hasFlag Version2

fromEither :: (Show s) => Either s a -> Hiss a
fromEither e = case e of
    Left s -> die (show s)
    Right a -> return a

die :: String -> Hiss a
die = left
