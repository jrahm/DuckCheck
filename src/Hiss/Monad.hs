module Hiss.Monad (Hiss, runHiss, hlog, verbose, isVersion2, hasFlag,
                   die, fromEither, hissLiftIO, runHissIO, StructuralType(..),
                   emptyType, singletonType, unionType, addFunction,
                   addClass, Function(..), HClass(..), emitWarning, getFunction,
                   getWarnings, getClass, typeHasAttr, fromSet, typeToString,
                   underContext, getGlobalFunction, isCompatibleWith, setTypeName,
                   getTypeName, toList, typeDifference
                   ) where

import Data.Maybe (fromMaybe)

import Control.Applicative
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

data StructuralType = Attributes {
      type_name :: Maybe String
    , type_attributes :: Set String
}

typeDifference :: StructuralType -> StructuralType -> Set String
typeDifference (Attributes _ s1) (Attributes _ s2) =  s1 Set.\\ s2

typeToString :: FunctionType -> String
typeToString (args, ret) = intercalate " -> " $ map show (args ++ [ret])

setTypeName :: String -> StructuralType -> StructuralType
setTypeName str typ = typ {type_name = Just str}

getTypeName :: StructuralType -> String
getTypeName (Attributes Nothing _) = "?"
getTypeName (Attributes (Just s) _) = s


fromSet :: Set String -> StructuralType
fromSet = Attributes Nothing

fromList :: [String] -> StructuralType
fromList = fromSet . Set.fromList

toList :: StructuralType -> [String]
toList (Attributes _ s) = Set.toList s

emptyType :: StructuralType
emptyType = Attributes Nothing Set.empty

singletonType :: String -> StructuralType
singletonType = Attributes Nothing . Set.singleton

addAttribute :: String -> StructuralType -> StructuralType
addAttribute str (Attributes n set) = Attributes n $ Set.insert str set

unionType :: StructuralType -> StructuralType -> StructuralType
unionType (Attributes n s1) (Attributes _ s2) = Attributes n (Set.union s1 s2)

typeHasAttr :: StructuralType -> String -> Bool
typeHasAttr (Attributes _ set) str = str `Set.member` set

isCompatibleWith :: StructuralType -> StructuralType -> Bool
isCompatibleWith (Attributes _ s1) (Attributes _ s2) | Set.null s1 = True
                                                     | otherwise =  s2 `Set.isSubsetOf` s1

instance Show StructuralType where
    show (Attributes name strs) =
        case Set.toList strs of
            [] -> "Any"
            l ->
                fromMaybe "" name ++ "{" ++ intercalate ", " l ++ "}"

type FunctionType = ([StructuralType], StructuralType)
data Function = Function
                 String -- name of function
                 FunctionType -- type of the function

data HClass = HClass
              String -- name of class
              StructuralType -- inferred structural type of class
              (Map String Function) -- member functions

data HissState e = HissState {
      flags :: Set Flag    -- command line flags

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

type Hiss e = EitherT String (StateT (HissState e) IO)

builtinGlobalFunctions :: Map String Function
builtinGlobalFunctions =
    Map.fromList $ map (\f@(Function name _) -> (name, f))[
          Function "print" ([emptyType], emptyType)
        , Function "len" ([fromList ["__len__"]], emptyType)
        , Function "str" ([fromList ["__str__"]], emptyType)
    ]

underContext :: String -> Hiss e a -> Hiss e a
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

-- underContext :: String -> Hiss e a -> Hiss e a
-- underContext str fn = do
--     last <- currentClassName <$> lift get
--     lift (modify $ \s -> s {currentClassName = Just str}) *>
--         fn <*
--             lift (modify $ \s -> s {currentClassName = last})

getWarnings :: HissState e -> [(String, e)]
getWarnings = warnings

getGlobalFunction :: String -> Hiss e (Maybe Function)
getGlobalFunction str = do
    st <- lift get
    return $ Map.lookup str (global_functions st)

getFunction :: String -> Hiss e (Maybe Function)
getFunction str = do
    st <- lift get
    return $
        (Map.lookup str =<< local_functions st) <|>
        Map.lookup str (global_functions st)

getClass :: String -> Hiss e (Maybe HClass)
getClass str = do
    st <- lift get
    return $
        (Map.lookup str =<< local_classes st) <|>
        Map.lookup str (global_classes st)

addFunction :: Function -> Hiss e ()
addFunction fn@(Function name _) =
    lift $ modify (\s -> s {global_functions = Map.insert name fn (global_functions s)})

addClass :: HClass -> Hiss e ()
addClass cl@(HClass name _ _) =
    lift $ modify (\s -> s {global_classes = Map.insert name cl (global_classes s)})

hissLiftIO :: IO a -> Hiss e a
hissLiftIO = lift . lift

emptyHissState :: Set Flag -> HissState e
emptyHissState flags = HissState flags builtinGlobalFunctions Map.empty [] Nothing Nothing

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
