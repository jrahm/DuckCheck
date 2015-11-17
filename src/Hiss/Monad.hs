module Hiss.Monad (Hiss, runHiss, hlog, verbose, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runHissIO, StructuralType(..),
                   emptyType, singletonType, unionType) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either

import Control.Monad (when)

import Hiss.Flags
import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map

import System.IO
import System.Exit (exitWith, ExitCode(ExitFailure))

import Data.List

newtype StructuralType = Attributes (Set String)

emptyType :: StructuralType
emptyType = Attributes Set.empty

singletonType :: String -> StructuralType
singletonType = Attributes . Set.singleton

addAttribute :: String -> StructuralType -> StructuralType
addAttribute str (Attributes set) = Attributes $ Set.insert str set

unionType :: StructuralType -> StructuralType -> StructuralType
unionType (Attributes s1) (Attributes s2) = Attributes (Set.union s1 s2)

instance Show StructuralType where
    show (Attributes strs) =
        case Set.toList strs of
            [] -> "Any"
            l -> "{" ++ intercalate ", " l ++ "}"

data HissState e = HissState {
      flags :: Set Flag    -- command line flags

      {- Function name to StructuralTypes of arguments to
       - StructuralType of return value -}
    , functions :: Map String ([StructuralType], StructuralType)
    , warnings :: [e]
}

type Hiss e = EitherT String (StateT (HissState e) IO)

hissLiftIO :: IO a -> Hiss e a
hissLiftIO = lift . lift

runHiss :: Set Flag -> Hiss e a -> IO (Either String a)
runHiss flags fn = evalStateT (runEitherT fn) (HissState flags Map.empty [])

runHissIO :: Set Flag -> Hiss e a -> IO a
runHissIO flags fn = do
    either <- evalStateT (runEitherT fn) (HissState flags Map.empty [])
    case either of
        Left s -> hPutStrLn stderr s >> exitWith (ExitFailure 1)
        Right a -> return a

emitWarning :: e -> Hiss e ()
emitWarning e = lift (modify $ \hs -> hs {warnings = e:warnings hs})

hlog :: String -> Hiss e ()
hlog str = lift $ lift $ putStrLn str

verbose :: String -> Hiss e ()
verbose str = do
    isVerbose <- (Verbose `elem`) . flags <$> lift get
    when isVerbose $ hlog str

hasFlag :: Flag -> Hiss e Bool
hasFlag f = (f `elem`) . flags <$> lift get

isVersion2 :: Hiss e Bool
isVersion2 = hasFlag Version2

fromEither :: (Show s) => Either s a -> Hiss e a
fromEither e = case e of
    Left s -> die (show s)
    Right a -> return a

die :: String -> Hiss e a
die = left
