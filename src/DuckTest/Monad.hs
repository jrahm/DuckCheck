{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DuckTest.Monad (DuckTest, runDuckTest, hlog, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runDuckTestIO, StructuralType(..),
                   emptyType, singletonType, unionType,
                    Function(..), HClass(..), emitWarning,
                   typeHasAttr, fromSet, typeToString, getWarnings,
                   isCompatibleWith, setTypeName, warn, findImport, makeImport,
                   getTypeName, typeDifference, saveState, LogLevel(..), (%%), getLogLevel
                   ) where

import System.FilePath
import System.Directory
import DuckTest.Internal.Common
import DuckTest.AST.Preprocess

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

    {- List of imported modules. -}
    , imports  :: Map [String] PyType
}

type DuckTest e = EitherT String (StateT (DuckTestState e) IO)

getLogLevel :: DuckTest e LogLevel
getLogLevel = lift (log_level <$> get)

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
emptyDuckTestState flags ll = DuckTestState flags ll mempty mempty

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

warn :: e -> (DuckTest e String) -> DuckTest e ()
warn pos mstr = mstr >>= (\str -> emitWarning str pos)

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
            maybe' importFile (emitWarning ("Unable to resolve import %s" ++ intercalate "." dottedlist) importPosition >> return Nothing) $ \filePath ->
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
