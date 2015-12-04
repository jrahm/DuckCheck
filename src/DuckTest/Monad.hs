{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  A module dedicated to the DuckTest monad. While this monad does use
  IO in the background, it is a attempted to encapsulate any IO the system
  will need.

  The DuckTest monad is an application level monad. Most all of the rest
  of the program operates under it, if operating under a monad at all.

  The monad is primarily responsible for keeping track of the flags passed
  to the executable as well as the warnings that are emitted.
 -}
module DuckTest.Monad (DuckTest, DuckTestState, runDuckTest, hlog, isVersion2, hasFlag,
                   die, fromEither, runDuckTestIO, dtReadFile,
                   emitWarning, getWarnings, ignore, warn, findImport, makeImport,
                   LogLevel(..), (%%), getLogLevel,
                   (%%!), runningInTerminal
                   ) where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Lazy
import DuckTest.AST.Preprocess
import DuckTest.Builtins
import DuckTest.Flags
import DuckTest.Internal.Common
import DuckTest.Types
import System.Directory
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.FilePath
import System.IO
import System.Posix.Terminal
import System.Posix.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

data DuckTestState e = DuckTestState {
      flags :: Set Flag    -- command line flags
    , logLevel :: LogLevel

    {- Warning collection list. For printing them out
     - at the end -}
    , warnings :: [(String, e)]

    , inTerminal :: Bool

    {- List of imported modules. -}
    , imports  :: Map [String] PyType
}

type DuckTest e = EitherT String (StateT (DuckTestState e) IO)

dtReadFile :: FilePath -> DuckTest e String
{-| Read a file under the DuckTest monad. -}
dtReadFile = hissLiftIO . readFile

getLogLevel :: DuckTest e LogLevel
{-| Returns the log level of the application. -}
getLogLevel = lift (logLevel <$> get)

getFirst :: (Monad m) => [a] -> (a -> m Bool) -> m (Maybe a)
getFirst (a:xs) f = do
    bool <- f a
    if bool then return (Just a)
        else getFirst xs f
getFirst [] _ = return Nothing

findImport :: [String] -> DuckTest e (Maybe FilePath)
{-| Try to find the filepath to the import given. The
    string list is a dotted identifier list. -}
findImport dotted = do
    let relativePath = intercalate "/" dotted
    let sitepath = ["", "/usr/lib/python3.5"]

    let possible = map ((++".py") . (</>relativePath)) sitepath

    getFirst possible $ \path -> do
       Trace %% "Try import file: " ++ path
       hissLiftIO $ doesFileExist path

getWarnings :: DuckTestState e -> [(String, e)]
{-| Get all the warnings from a state. -}
getWarnings = warnings


hissLiftIO :: IO a -> DuckTest e a
hissLiftIO = lift . lift

emptyDuckTestState :: Set Flag -> LogLevel -> Bool -> DuckTestState e
emptyDuckTestState flags ll term = DuckTestState flags ll mempty term $
    Map.singleton ["sys"] sysType

runDuckTest :: Set Flag -> LogLevel -> DuckTest e a -> IO (Either String a)
{-| Run something under the duck test monad given the set of command
 - line flags and the level to log at. -}
runDuckTest flags ll fn = do
    isTerm <- queryTerminal (Fd 1)
    evalStateT (runEitherT fn) (emptyDuckTestState flags ll isTerm)

runDuckTestIO :: Set Flag -> LogLevel -> DuckTest e a -> IO (DuckTestState e)
runDuckTestIO flags ll fn = do
    isTerm <- queryTerminal (Fd 1)
    flip execStateT (emptyDuckTestState flags ll isTerm) $ do

            either <- runEitherT fn

            case either of
                Left s -> liftIO (hPutStrLn stderr s >> exitWith (ExitFailure 1))
                Right s -> return s

emitWarning :: String -> e -> DuckTest e ()
emitWarning str e = do
    Trace %% "Warning emitted: " ++ str
    lift (modify $ \hs -> hs {warnings = (str, e):warnings hs})

warn :: e -> DuckTest e String -> DuckTest e ()
warn pos mstr = mstr >>= (`emitWarning`pos)

hlog :: String -> DuckTest e ()
hlog str = lift $ lift $ putStrLn str

(%%) :: LogLevel -> String -> DuckTest e ()
(%%) ll str = do
    level <- logLevel <$> lift get
    when (ll >= level) $
        forM_ (lines str) $ \line ->
            hlog $ printf "[%s] - %s" (show ll) line

(%%!) :: LogLevel -> DuckTest e String -> DuckTest e ()
(%%!) ll str = str >>= (%%) ll

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

ignore :: DuckTest e a -> DuckTest e a
{-| Run the function fn, but ignore all warnings emitted from
 - that function. -}
ignore fn = do
    before <- lift (warnings <$> get)
    fn <* lift (modify (\s -> s {warnings = before}))

makeImport :: SrcSpan ->
              [String] ->
              (FilePath  -> DuckTest SrcSpan (Maybe (ModuleSpan, [Token]))) ->
              ([Statement SrcSpan] -> DuckTest SrcSpan PyType) -> DuckTest SrcSpan (Maybe PyType)
makeImport importPosition dottedlist parser checker = do
    Debug %% printf "Make import %s" (show dottedlist)
    maybePyType <- Map.lookup dottedlist <$> (imports <$> lift get)
    case maybePyType of
        Just typ -> do
            Debug %% printf "Use cached"
            return (Just typ)
        Nothing -> do
            importFile <- findImport dottedlist
            maybe' importFile (emitWarning ("Unable to resolve import " ++ intercalate "." dottedlist) importPosition >> return Nothing) $ \filePath ->
                (>>=) (parser filePath) $ mapM $
                    \(Module stmts', _) -> do
                        let stmts = preprocess stmts'

                        lift $ modify $ \state ->
                            state {imports = Map.insert dottedlist Any (imports state)}

                        modType <- checker stmts

                        lift $ modify $ \state ->
                            state {imports = Map.insert dottedlist modType (imports state)}

                        return modType
infixl 1 %%

runningInTerminal :: DuckTest e Bool
runningInTerminal = lift $ inTerminal <$> get
