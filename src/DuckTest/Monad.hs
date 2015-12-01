module DuckTest.Monad (DuckTest, runDuckTest, hlog, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runDuckTestIO, StructuralType(..),
                   emptyType, singletonType, unionType,
                    Function(..), HClass(..), emitWarning,
                   typeHasAttr, fromSet, typeToString, getWarnings,
                   isCompatibleWith, setTypeName, warn, warnTypeError, findImport,
                   getTypeName, typeDifference, saveState, LogLevel(..), (%%)
                   ) where

import System.FilePath
import System.Directory
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

import DuckTest.Types


data DuckTestState e = DuckTestState {
      flags :: Set Flag    -- command line flags
    , log_level :: LogLevel

    {- Warning collection list. For printing them out
     - at the end -}
    , warnings :: [(String, e)]
}

type DuckTest e = EitherT String (StateT (DuckTestState e) IO)

getFirst :: (Monad m) => [a] -> (a -> m Bool) -> m (Maybe a)
getFirst (a:xs) f = do
    bool <- f a
    if bool then return (Just a)
        else getFirst xs f
getFirst [] _ = return Nothing

findImport :: [String] -> DuckTest e (Maybe FilePath)
findImport dotted = do
    let relativePath = intercalate "/" dotted
    let sitepath = ["", "/usr/lib/python3.5"]

    let possible = map ((++".py") . (</>relativePath)) sitepath

    getFirst possible $ \path -> do
       Trace %% "Try import file: " ++ path
       hissLiftIO $ doesFileExist path

saveState :: DuckTest e a -> DuckTest e a
saveState fn = do
    st <- lift get
    ret <- fn
    st' <- lift get
    lift $ put $ st {warnings = warnings st'}
    return ret

getWarnings :: DuckTestState e -> [(String, e)]
getWarnings = warnings


hissLiftIO :: IO a -> DuckTest e a
hissLiftIO = lift . lift

emptyDuckTestState :: Set Flag -> LogLevel -> DuckTestState e
emptyDuckTestState flags ll = DuckTestState flags ll []

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
emitWarning str e = do
    Trace %% "Warning emitted: " ++ str
    lift (modify $ \hs -> hs {warnings = (str, e):warnings hs})

warn = emitWarning

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

warnTypeError :: e -> TypeError -> DuckTest e ()
warnTypeError pos (Incompatible t1 t2) =
    warn (printf "Incompatible types %s and %s" (show t1) (show t2)) pos

warnTypeError pos (Difference name dif) =
    warn (printf "Type %s missing attributes needed: %s" name (intercalate ", " (map (intercalate ".") dif))) pos

infixl 1 %%
