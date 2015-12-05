{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module DuckTest.Types
    (union, intersection, difference, difference2, prettyType, prettyType',
     getCallType, PyType(..), (><), (<>), fromList, mkAlpha,
     TypeError(..), getAttribute, stripAlpha, matchType, UnionType(..),
     IntersectionType(..), unwrap, singleton, liftFromDotList, isVoid,
     instanceTypeFromStatic, setAttribute)
    where

import DuckTest.Internal.Common hiding (union, (<>))

import qualified Data.Map as Map

import Control.Arrow
import Control.Monad.Writer.Lazy hiding (Any, (<>))

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
              | Alpha PyType
              | Void

instance Show PyType where
    show (Scalar name strs) =
        case Map.toList strs of
            [] -> "Void"
            l ->
                fromMaybe "" name ++ "{" ++ intercalate ", " (map (\(str, typ) -> str ++ " :: " ++ show typ) l) ++ "}"
    show (Functional args ret) = "(" ++ intercalate "," (map show args) ++ ") -> " ++ show ret
    show Any = "Any"
    show (Alpha _) = "alpha"
    show Void = "Void"

fromList :: Maybe String -> [(String, PyType)] -> PyType
fromList m = Scalar m . Map.fromList

singleton :: String -> PyType -> PyType
singleton str = Scalar Nothing . Map.singleton str

union :: PyType -> PyType -> PyType
{-| The union of two PyTypes. This returs a third PyType that is
 - "larger" than the previous two. That means it matches more things,
 - but it is more restrictive to match against. It is always at least
 - as hard to match against the union of t1 and t2 than it is to match
 - against t1 and t2 individually
 -
 - The union is used to infer the type of variables by inference primarily -}
-- union x y = trace (prettyType x ++ " u " ++ prettyType y) $ union' x y
union = union'
    where
    union' :: PyType -> PyType -> PyType
    union' Any _ = Any
    union' _ Any = Any
    union' t Void = t
    union' Void t = t
    union' (Scalar s1 m1) (Scalar s2 m2) = Scalar (unionStr s1 s2) $ Map.unionWith union m1 m2
        where
            unionStr Nothing Nothing = Nothing
            unionStr s1' s2' | s1' == s2'  = s1'
                           | otherwise = Just $ printf "(%s)&(%s)" (fromMaybe "?" s1) (fromMaybe "?" s2)

    union' sc@Scalar {} f@Functional {} = sc `union` singleton "__call__" f
    union' (Functional args1 ret1) (Functional args2 ret2) =
        Functional (zipWith (\(s1, t1) (_, t2) -> (s1, t1 `intersection` t2)) args1 args2)
                   (ret1 `union` ret2)
    union' f@Functional {} t = t `union` f
    union' (Alpha t1) (Alpha t2) = Alpha $ union t1 t2
    union' (Alpha s1) t2 = Alpha (s1 `union` t2)
    union' t1 (Alpha s1) = Alpha (s1 `union` t1)

intersection :: PyType -> PyType -> PyType
{-| The intersection of two types. The oppsite of the union is true
   for these types. The intersection type is smaller than its constituents,
   and is there fore harder to match against other types, but this type
   is easier for others to match against. The intersection of types is
   used to calculate the return type in branching Suites. -}
-- intersection x y = trace (prettyType x ++ " n " ++ prettyType y) $ intersection' x y
intersection = intersection'
    where
    intersection' :: PyType -> PyType -> PyType
    intersection' Any t = t
    intersection' t Any = t
    intersection' Void _ = Void
    intersection' _ Void = Void
    intersection' (Scalar s1 m1) (Scalar s2 m2) = Scalar (interStr s1 s2) $ Map.intersectionWith intersection m1 m2
        where
            interStr Nothing Nothing = Nothing
            interStr s1' s2' | s1' == s2' = s1'
                             | otherwise = Just $ printf "(%s)|(%s)" (fromMaybe "?" s1) (fromMaybe "?" s2)
    intersection' sc@Scalar {} f@Functional {} = intersection sc (singleton "__call__" f)
    intersection' (Functional args1 ret1) (Functional args2 ret2) =
        Functional (zipWith (\(s1, t1) (_, t2) -> (s1, t1 `union` t2)) args1 args2)
                   (intersection ret1 ret2)
    intersection' f@Functional {} t = intersection t f
    intersection' (Alpha t1) (Alpha t2) = Alpha $ intersection t1 t2
    intersection' (Alpha t1) t2 = Alpha $ intersection t1 t2
    intersection' t1 (Alpha t2) = Alpha $ intersection t1 t2

    -- difference x y = trace (prettyType x ++ " - " ++ prettyType y) $ difference' x y
difference :: PyType -> PyType -> PyType
{-| The difference of a type returns a type that can act like the
 - first, but not the second. Difference is not a very useful
 - operator for actually constructing types, however, it is extremely
 - useful for checking to see if two types are compatible. That is,
 - it can check to see if the first type is smaller or larger than
 - the second type. -}
difference = difference' difference
difference' :: (PyType -> PyType -> PyType) -> PyType -> PyType -> PyType
difference' _ Any Any = Void
difference' _ Any _ = Any
difference' _ _ Any = Void
difference' _ t Void = t
difference' _ Void _ = Void
difference' dif (Scalar _ m1) (Scalar _ m2) =
    toVoid $ Scalar Nothing $ Map.differenceWith fn m1 m2
    where
        fn t1 t2 = if isVoid (dif t1 t2) then
                    Nothing else Just (dif t1 t2)
difference' dif sc@Scalar {} f@Functional {} = dif sc (singleton "__call__" f)
difference' dif (Functional args1 ret1) (Functional args2 ret2) =
    let almost = Functional (zipWith (\(s1, t1) (_, t2) -> (s1, dif t1 t2)) args1 args2)
                    (dif ret1 ret2)
                    in if isVoidFunction almost then Void else almost
difference' dif f@Functional {} t = dif t f
difference' dif (Alpha t1) (Alpha t2) = Alpha $ dif t1 t2
difference' dif (Alpha s1) t2 = Alpha $ dif s1 t2
difference' dif t1 (Alpha s1) = Alpha $ dif t1 s1

difference2 :: PyType -> PyType -> PyType
difference2 Alpha {} _ = Void
difference2 _ Alpha {} = Void
difference2 Any _ = Void
difference2 _ Any = Void
difference2 t1 t2 = difference' difference2 t1 t2

isVoid :: PyType -> Bool
{-| returns true if the given type is void. Theoretically speaking, only the Void
 - type is supposed to be void, but with how the algebra is constructed, it is
 - possible to get elemests like the empty Scaalar and the alpha of the empty
 - scalar. Both of these are still void (and theoretically equal to void). -}
isVoid t = case t of
    (Alpha t') -> isVoid' t'
    _ -> isVoid' t
    where
        isVoid' Void = True
        isVoid' (Scalar _ m) | Map.null m = True
        isVoid' (Alpha _) = True
        isVoid' _ = False

toVoid :: PyType -> PyType
{-| If a type is void, then returns Void, else returns the type unchanged -}
toVoid v | isVoid v = Void
toVoid x = x

isCompatibleAs :: PyType -> PyType -> PyType
{-| Check ot see if type `larger` is compatible with `smaller`.
    While this function theorectcally check so smaller - larger == Void,
    this function will add the guard making anything compatible with the
    Any type -}
isCompatibleAs Any _ = Void
isCompatibleAs _ Any = Void
isCompatibleAs smaller larger = difference2 smaller larger

getAttribute :: String -> PyType -> Maybe PyType
{-| return the attribute of a type, or Nothing if
 - there is no such attribute. -}
getAttribute att (Scalar _ map') = Map.lookup att map'
getAttribute "__call__" f@Functional {} = Just f
getAttribute att (Alpha t) = getAttribute att t
getAttribute _ _ = Nothing

setAttribute :: String -> PyType -> PyType -> PyType
{-| return the attribute of a type, or Nothing if
 - there is no such attribute. -}
setAttribute att typ (Scalar _ map') = Scalar Nothing $ Map.insert att typ map'
setAttribute att typ t@Functional {} = setAttribute att typ $ Scalar Nothing $ Map.singleton "__call__" t
setAttribute att typ (Alpha a) = Alpha (setAttribute att typ a)
setAttribute _ _ Any = Any
setAttribute att typ Void = singleton att typ

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

(><) :: PyType -> PyType -> PyType
(><) = intersection

(<>) :: PyType -> PyType -> PyType
(<>) = union

mkAlpha :: PyType -> PyType
mkAlpha = Alpha

liftFromDotList :: [String] -> PyType -> PyType
liftFromDotList list init' = foldr singleton init' list

prettyType :: PyType -> String
prettyType = prettyType' False

prettyType' :: Bool -> PyType -> String
prettyType' b (Alpha s) = "alpha " ++ prettyType'' b s
prettyType' b t = prettyType'' b t

prettyType'' :: Bool -> PyType -> String
prettyType'' descend typ = execWriter $ prettyType_ 0 typ
    where
          prettyType_ indent (Scalar (Just name) s) = tell name >> when descend (tell " " >> prettyType_ indent (Scalar Nothing s))
          prettyType_ indent (Scalar Nothing attrs) = do
                tell "{ "
                let lst = Map.toList attrs
                unless (null lst) $  do
                    let ((attr, atttyp):t) = lst
                    let attrplus = attr ++ " :: "
                    tell attrplus
                    prettyType_ (indent + length attrplus + 2) atttyp
                    forM_ t $ \(forattr, foratttyp) -> do
                        tell "\n"
                        tab indent
                        let forattrplus = ", " ++ forattr ++ " :: "
                        tell forattrplus
                        prettyType_ (indent + length forattrplus) foratttyp
                tell "} "

          prettyType_ indent (Functional args ret) = do
             tell "( "
             unless (null args) $ do
                 let ((_, h):t) = args
                 prettyType_ (indent + 2) h
                 forM_ t $ \(_, fortyp) -> do
                     tell "\n, "
                     prettyType_ (indent + 2) fortyp
             tell ") -> "
             prettyType_ (indent + 1) ret

          prettyType_ _ Any = tell "Any"
          prettyType_ _ (Alpha _) = tell "alpha"

          prettyType_ _ Void = tell "Void"

          tab :: Int -> Writer String ()
          tab indent = forM_ [1..indent] $ const $ tell " "

data TypeError = Incompatible PyType PyType | Difference PyType PyType (Map String PyType)

isVoidFunction :: PyType -> Bool
isVoidFunction (Functional args ret) = all (isVoid . snd) (("",ret):args)
isVoidFunction _ = False

stripAlpha :: PyType -> PyType
stripAlpha a@Alpha {} = stripAlpha' a
stripAlpha (Functional args ret) = Functional (map (second stripAlpha) args) (stripAlpha ret)
stripAlpha (Scalar n m) = Scalar n $ Map.map stripAlpha m
stripAlpha x = x

stripAlpha' :: PyType -> PyType
stripAlpha' (Alpha a) = stripAlpha' a
stripAlpha' x = x

{- No news is good news. Check to see if t1 is smaller than t2 -}
matchType :: PyType -> PyType -> Maybe TypeError
matchType t1 t2 =
        case isCompatibleAs t1 t2 of
            t | isVoid t -> Nothing
            (Scalar _ m) -> Just (Difference t1 t2 m)
            (Alpha (Scalar _ m)) -> Just (Difference t1 t2 m)
            _ -> Just (Incompatible t1 t2)

getCallType :: PyType -> Maybe PyType
getCallType t@Functional {} = Just t
getCallType (Scalar _ m) = Map.lookup "__call__" m
getCallType (Alpha a) = getCallType a
getCallType _ = Nothing

instanceTypeFromStatic :: PyType -> Maybe PyType
instanceTypeFromStatic (Functional _ r) = Just r
instanceTypeFromStatic (Scalar _ m) =
    case Map.lookup "__call__" m of
        Just (Functional _ ret) -> Just ret
        _ -> Nothing
instanceTypeFromStatic _ = Nothing
