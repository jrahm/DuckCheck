{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module DuckTest.Types where

import DuckTest.Internal.Common hiding (union)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

import Control.Arrow
import Control.Monad.Writer.Lazy hiding (Any)

{-
 - This is a data type representing an inferred type in Python. THere
 - are a few different constructers. The first is a Scalar. This simply
 - has a structural type (i.e. essentially a mapping of attributes to types,
 - recursively). Second, we have the functional type, this has a list of
 - arguments and their types to the return type of the function, recursively.
 - Next, there is the Any type, this type will typecheck with anything. It
 - is how the inference engine can deal with errors without them cascading.
 - Finally, the Any type is considered a 'reference'. It used to refer
 - to a type recursively by name so as to not cause the checking functions
 - to recurse forever.
 -
 - For the alpha type - consider a linked list. This may have type
 - Scalar $ StructuralType "LinkedList"
 -  {data :: any, next :: {data :: any, next :: {data :: any, next :: {...}}}}
 - on and on forever. So to counter this, we use the alpha type to make this
 - Scalar $ StructuralType "LinkedList" {data :: any, next :: Alpha "LinkedList" _}
 - The structure in the data is actually infinitely recursive, but thanks to haskell's
 - laziness, we don't have to worry about this.
 -}

data PyType =   Scalar (Maybe String) (Map String PyType)
              | Functional [(String, PyType)] PyType
              | Any
              | Alpha String PyType
              | Void

instance Show PyType where
    show (Scalar name strs) =
        case Map.toList strs of
            [] -> "Void"
            l ->
                fromMaybe "" name ++ "{" ++ intercalate ", " (map (\(str, typ) -> str ++ " :: " ++ show typ) l) ++ "}"
    show (Functional args ret) = "(" ++ intercalate "," (map show args) ++ ") -> " ++ show ret
    show Any = "Any"
    show (Alpha name _) = "alpha " ++ name
    show Void = "Void"

fromList :: Maybe String -> [(String, PyType)] -> PyType
fromList m = Scalar m . Map.fromList

singleton :: String -> PyType -> PyType
singleton str = Scalar Nothing . Map.singleton str

singletonAny :: String -> PyType
singletonAny = flip singleton Any

-- union x y = trace (prettyType x ++ " u " ++ prettyType y) $ union' x y
union = union'
union' :: PyType -> PyType -> PyType
union' Any _ = Any
union' _ Any = Any
union' t Void = t
union' Void t = t
union' (Scalar _ m1) (Scalar _ m2) = Scalar Nothing $ Map.unionWith union m1 m2
union' sc@(Scalar {}) f@(Functional {}) = sc `union` singleton "__call__" f
union' (Functional args1 ret1) (Functional args2 ret2) =
    Functional (zipWith (\(s1, t1) (_, t2) -> (s1, union t1 t2)) args1 args2)
               (union ret1 ret2)
union' f@(Functional {}) t = t `union` f
union' t1@(Alpha {}) t2 | hasSameName t1 t2 = t1
union' t1 t2@(Alpha {}) | hasSameName t1 t2 = t2
union' (Alpha _ t1) (Alpha _ t2) = Alpha "" $ union t1 t2
union' (Alpha _ s1) t2 = Alpha "" (union s1 t2)
union' t1 (Alpha _ s1) = Alpha "" (union s1 t1)

-- intersection x y = trace (prettyType x ++ " n " ++ prettyType y) $ intersection' x y
intersection = intersection'
intersection' :: PyType -> PyType -> PyType
intersection' Any t = t
intersection' t Any = t
intersection' Void _ = Void
intersection' _ Void = Void
intersection' (Scalar _ m1) (Scalar _ m2) = Scalar Nothing $ Map.intersectionWith intersection m1 m2
intersection' sc@(Scalar {}) f@(Functional {}) = intersection sc (singleton "__call__" f)
intersection' (Functional args1 ret1) (Functional args2 ret2) =
    Functional (zipWith (\(s1, t1) (_, t2) -> (s1, intersection t1 t2)) args1 args2)
               (intersection ret1 ret2)
intersection' f@(Functional {}) t = intersection t f
intersection' (Alpha _ t1) (Alpha _ t2) = Alpha "" $ intersection t1 t2
intersection' (Alpha _ t1) t2 = Alpha "" $ intersection t1 t2
intersection' t1 (Alpha _ t2) = Alpha "" $ intersection t1 t2

-- difference x y = trace (prettyType x ++ " - " ++ prettyType y) $ difference' x y
difference = difference'

difference' :: PyType -> PyType -> PyType
difference' Any Any = Void
difference' Any _ = Any
difference' _ Any = Void
difference' t Void = t
difference' Void _ = Void
difference' (Scalar _ m1) (Scalar _ m2) =
    toVoid $ Scalar Nothing $ Map.differenceWith fn m1 m2
    where
        fn t1 t2 = if isVoid (difference t1 t2) then
                    Nothing else Just (difference t1 t2)
difference' sc@(Scalar {}) f@(Functional {}) = difference sc (singleton "__call__" f)
difference' (Functional args1 ret1) (Functional args2 ret2) =
    let almost = Functional (zipWith (\(s1, t1) (_, t2) -> (s1, difference t1 t2)) args1 args2)
                    (difference ret1 ret2)
                    in if isVoidFunction almost then Void else almost

difference' f@(Functional {}) t = difference t f
difference' t1@(Alpha {}) t2 | hasSameName t1 t2 = Void
difference' t1 t2@(Alpha {}) | hasSameName t1 t2 = Void
difference' (Alpha _ t1) (Alpha _ t2) = Alpha "" $ difference t1 t2
difference' (Alpha _ s1) t2 = Alpha "" $ difference s1 t2
difference' t1 (Alpha _ s1) = Alpha "" $ difference t1 s1

hasSameName :: PyType -> PyType -> Bool
hasSameName (Alpha s1 _) (Alpha s2 _) | s1 == s2 = True
hasSameName (Alpha s1 _) (Scalar (Just s2) _) | s1 == s2 = True
hasSameName (Scalar (Just s2) _) (Alpha s1 _) | s1 == s2 = True
hasSameName _ _ = False

isVoid :: PyType -> Bool
isVoid Void = True
isVoid (Scalar _ m) | Map.null m = True
isVoid (Alpha _ t) = isVoid t
isVoid _ = False

toVoid :: PyType -> PyType
toVoid v | isVoid v = Void
toVoid x = x

isCompatibleAs :: PyType -> PyType -> Bool
isCompatibleAs Any _ = True
isCompatibleAs _ Any = True
isCompatibleAs smaller larger = isVoid $ difference smaller larger

getAttribute :: String -> PyType -> Maybe PyType
getAttribute att (Scalar _ map) = Map.lookup att map
getAttribute "__call__" f@(Functional {}) = Just f
getAttribute att (Alpha _ t) = getAttribute att t
getAttribute _ _ = Nothing

newtype UnionType = Union PyType
instance Monoid UnionType where
    mempty = Union Void
    mappend (Union a) (Union b) = Union $ union a b

newtype IntersectionType = Intersect PyType
instance Monoid IntersectionType where
    mempty = Intersect Any
    mappend (Intersect a) (Intersect b) = Intersect $ intersection a b

class Unwrapbable a b | a -> b where
    unwrap :: a -> b

instance Unwrapbable UnionType PyType where
    unwrap (Union x) = x

instance Unwrapbable IntersectionType PyType where
    unwrap (Intersect x) = x

(><) = intersection
(<>) = union

mkAlpha :: PyType -> PyType
mkAlpha s@(Scalar name _) = Alpha (fromMaybe "" name) s
mkAlpha s = Alpha "" s

liftFromDotList :: [String] -> PyType -> PyType
liftFromDotList list init = foldr singleton init list

prettyType :: PyType -> String
prettyType = prettyType' False

prettyType' b (Alpha "" s) = "alpha " ++ prettyType'' b s
prettyType' b t = prettyType'' b t

prettyType'' :: Bool -> PyType -> String
prettyType'' descend typ = execWriter $ prettyType' 0 typ
    where
          prettyType' indent (Scalar (Just name) s) = tell name >> when descend (tell " " >> prettyType' indent (Scalar Nothing s))
          prettyType' indent (Scalar Nothing attrs) = do
                tell "{ "
                let lst = Map.toList attrs
                unless (null lst) $  do
                    let ((attr, typ):t) = lst
                    let attrplus = attr ++ " :: "
                    tell attrplus
                    prettyType' (indent + length attrplus + 2) typ
                    forM_ t $ \(attr, typ) -> do
                        tell "\n"
                        tab indent
                        let attrplus = ", " ++ attr ++ " :: "
                        tell attrplus
                        prettyType' (indent + length attrplus) typ
                tell "} "

          prettyType' indent (Functional args ret) = do
             tell "( "
             unless (null args) $ do
                 let ((_, h):t) = args
                 prettyType' (indent + 2) h
                 forM_ t $ \(_, typ) -> do
                     tell "\n, "
                     prettyType' (indent + 2) typ
             tell ") -> "
             prettyType' (indent + 1) ret

          prettyType' _ Any = tell "Any"
          prettyType' _ (Alpha nam _) = tell $ "alpha " ++ nam

          prettyType' _ Void = tell "Void"

          tab :: Int -> Writer String ()
          tab indent = forM_ [1..indent] $ const $ tell " "

data TypeError = Incompatible PyType PyType | Difference PyType PyType (Map String PyType)

isVoidFunction :: PyType -> Bool
isVoidFunction (Functional args ret) = all (isVoid . snd) (("",ret):args)
isVoidFunction _ = False

stripAlpha :: PyType -> PyType
stripAlpha a@(Alpha {}) = stripAlpha' a
stripAlpha (Functional args ret) = Functional (map (second stripAlpha') args) (stripAlpha' ret)
stripAlpha (Scalar n map) = Scalar n $ Map.map stripAlpha map
stripAlpha x = x

stripAlpha' (Alpha _ a) = stripAlpha' a
stripAlpha' x = x

{- No news is good news. Check to see if t1 is smaller than t2 -}
matchType :: PyType -> PyType -> Maybe TypeError
matchType Any _ = Nothing
matchType _ Any = Nothing
matchType t1 t2 =
    let dif = difference t1 t2 in
    if isVoid dif || isVoidFunction dif then Nothing else
        case dif of
            (Scalar _ map) -> Just (Difference t1 t2 map)
            (Alpha _ (Scalar _ map)) -> Just (Difference t1 t2 map)
            _ -> Just (Incompatible t1 t2)

setTypeName :: String -> PyType -> PyType
setTypeName s (Scalar _ m) = Scalar (Just s) m
setTypeName _ t = t

getCallType :: PyType -> Maybe PyType
getCallType t@(Functional {}) = Just t
getCallType (Scalar _ m) = Map.lookup "__call__" m
getCallType (Alpha _ a) = getCallType a
getCallType _ = Nothing
