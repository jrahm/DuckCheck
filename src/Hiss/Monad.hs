module Hiss.Monad (Hiss, runHiss, hlog, verbose, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runHissIO, StructuralType(..),
                   emptyType, singletonType, unionType, addFunction,
                   addClass, Function(..), Class(..), emitWarning, getFunction,
                   getWarnings
                   ) where

import Control.Monad.IO.Class
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

type FunctionType = ([StructuralType], StructuralType)
data Function = Function
                 String -- name of function
                 FunctionType -- type of the function

data Class = Class
              String -- name of class
              StructuralType -- inferred structural type of class
              (Map String Function) -- member functions

data HissState e = HissState {
      flags :: Set Flag    -- command line flags

      {- Function name to StructuralTypes of arguments to
       - StructuralType of return value -}
    , functions :: Map String Function
    , classes :: Map String Class

    {- Warning collection list. For printing them out
     - at the end -}
    , warnings :: [(String, e)]
}

type Hiss e = EitherT String (StateT (HissState e) IO)

getWarnings :: HissState e -> [(String, e)]
getWarnings = warnings

getFunction :: String -> Hiss e (Maybe Function)
getFunction str =
    (Map.lookup str . functions) <$> lift get

getClass :: String -> Hiss e (Maybe Class)
getClass str =
    (Map.lookup str . classes) <$> lift get

addFunction :: Function -> Hiss e ()
addFunction fn@(Function name _) =
    lift $ modify (\s -> s {functions = Map.insert name fn (functions s)})

addClass :: Class -> Hiss e ()
addClass cl@(Class name _ _) =
    lift $ modify (\s -> s {classes = Map.insert name cl (classes s)})

hissLiftIO :: IO a -> Hiss e a
hissLiftIO = lift . lift

emptyHissState :: Set Flag -> HissState e
emptyHissState flags = HissState flags Map.empty Map.empty []

runHiss :: Set Flag -> Hiss e a -> IO (Either String a)
runHiss flags fn = evalStateT (runEitherT fn) (emptyHissState flags)

runHissIO :: Set Flag -> Hiss e a -> IO (HissState e)
runHissIO flags fn =
    flip execStateT (emptyHissState flags) $ do

            either <- runEitherT fn

            case either of
                Left s -> liftIO (hPutStrLn stderr s >> exitWith (ExitFailure 1))
                Right s -> return s

emitWarning :: String -> e -> Hiss e ()
emitWarning str e = lift (modify $ \hs -> hs {warnings = (str, e):warnings hs})

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
