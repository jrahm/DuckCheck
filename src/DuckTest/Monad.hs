module DuckTest.Monad (DuckTest, runDuckTest, hlog, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runDuckTestIO, StructuralType(..),
                   emptyType, singletonType, unionType, addFunction,
                   addClass, Function(..), HClass(..), emitWarning, getFunction,
                   getWarnings, getClass, typeHasAttr, fromSet, typeToString,
                   underContext, getGlobalFunction, isCompatibleWith, setTypeName,
                   getTypeName, toList, typeDifference, saveState, LogLevel(..), (%%)
                   ) where

import DuckTest.Internal.Common

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either

import DuckTest.Flags

import qualified Data.Set as Set
import qualified Data.Map as Map

import System.IO
import System.Exit (exitWith, ExitCode(ExitFailure))

import DuckTest.Builtins
import DuckTest.Types


data DuckTestState e = DuckTestState {
      flags :: Set Flag    -- command line flags
    , log_level :: LogLevel

      {- Function name to StructuralTypes of arguments to
       - StructuralType of return value -}
    , global_functions :: Map String Function
    , global_classes :: Map String HClass

    {- Warning collection list. For printing them out
     - at the end -}
    , warnings :: [(String, e)]

    , local_functions :: Maybe (Map String Function)
    , local_classes :: Maybe (Map String HClass)
}

type DuckTest e = EitherT String (StateT (DuckTestState e) IO)

saveState :: DuckTest e a -> DuckTest e a
saveState fn = do
    st <- lift get
    ret <- fn
    st' <- lift get
    lift $ put $ st {warnings = warnings st'}
    return ret

underContext :: String -> DuckTest e a -> DuckTest e a
underContext str fn = do
    st <- lift get
    let (oldFns, oldCls) =
         (local_functions st, local_classes st)

    newClass <- getClass str
    case newClass of
        Nothing -> fn
        Just (HClass _ _ map) ->
            lift (put (st {local_functions = Just map})) *> fn <*
                lift (modify (\st' -> st' {local_functions = oldFns}))

-- underContext :: String -> DuckTest e a -> DuckTest e a
-- underContext str fn = do
--     last <- currentClassName <$> lift get
--     lift (modify $ \s -> s {currentClassName = Just str}) *>
--         fn <*
--             lift (modify $ \s -> s {currentClassName = last})

getWarnings :: DuckTestState e -> [(String, e)]
getWarnings = warnings

getGlobalFunction :: String -> DuckTest e (Maybe Function)
getGlobalFunction str = do
    st <- lift get
    return $ Map.lookup str (global_functions st)

getFunction :: String -> DuckTest e (Maybe Function)
getFunction str = do
    st <- lift get
    return $
        (Map.lookup str =<< local_functions st) <|>
        Map.lookup str (global_functions st)

getClass :: String -> DuckTest e (Maybe HClass)
getClass str = do
    st <- lift get
    return $
        (Map.lookup str =<< local_classes st) <|>
        Map.lookup str (global_classes st)

addFunction :: Function -> DuckTest e ()
addFunction fn@(Function name _) =
    lift $ modify (\s -> s {global_functions = Map.insert name fn (global_functions s)})

addClass :: HClass -> DuckTest e ()
addClass cl@(HClass name _ _) =
    lift $ modify (\s -> s {global_classes = Map.insert name cl (global_classes s)})

hissLiftIO :: IO a -> DuckTest e a
hissLiftIO = lift . lift

emptyDuckTestState :: Set Flag -> LogLevel -> DuckTestState e
emptyDuckTestState flags ll = DuckTestState flags ll builtinGlobalFunctions builtinGlobalClasses [] Nothing Nothing

runDuckTest :: Set Flag -> LogLevel -> DuckTest e a -> IO (Either String a)
runDuckTest flags ll fn = evalStateT (runEitherT fn) (emptyDuckTestState flags ll)

runDuckTestIO :: Set Flag -> LogLevel -> DuckTest e a -> IO (DuckTestState e)
runDuckTestIO flags ll fn =
    flip execStateT (emptyDuckTestState flags ll) $ do

            either <- runEitherT fn

            case either of
                Left s -> liftIO (hPutStrLn stderr s >> exitWith (ExitFailure 1))
                Right s -> return s

emitWarning :: String -> e -> DuckTest e ()
emitWarning str e = lift (modify $ \hs -> hs {warnings = (str, e):warnings hs})

hlog :: String -> DuckTest e ()
hlog str = lift $ lift $ putStrLn str

(%%) :: LogLevel -> String -> DuckTest e ()
(%%) ll str = do
    level <- log_level <$> lift get
    when (ll >= level) $
        forM_ (lines str) $ \line ->
            hlog $ printf "[%s] - %s" (show ll) line

hasFlag :: Flag -> DuckTest e Bool
hasFlag f = (f `elem`) . flags <$> lift get

isVersion2 :: DuckTest e Bool
isVersion2 = hasFlag Version2

fromEither :: (Show s) => Either s a -> DuckTest e a
fromEither e = case e of
    Left s -> die (show s)
    Right a -> return a

die :: String -> DuckTest e a
die = left

infixl 1 %%
