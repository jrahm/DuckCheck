{-# LANGUAGE TupleSections #-}
module DuckTest.Types where

import DuckTest.Internal.Common

import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

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
data PyType = Scalar StructuralType |
              Functional [(String, PyType)] PyType |
              Any |
              Alpha String PyType |
              Void

instance Show PyType where
    show (Scalar st) = show st
    show (Functional args ret) = "(" ++ intercalate "," (map show args) ++ ") -> " ++ show ret
    show Any = "Any"
    show (Alpha name _) = "alpha " ++ name
    show Void = "Void"

instance Monoid PyType where
    mempty = Void

    mappend Any _ = Any
    mappend _ Any = Any
    mappend Void x = x
    mappend x Void = x
    mappend (Scalar st) ty@(Functional {}) =
        {- In Python, if we observe a variable being used as a function and a scalar,
         - then that variable is a scalar with a call function -}
        Scalar (addAttribute "__call__" ty st)
    mappend (Functional p1 r1) (Functional p2 r2) = Functional (zipWith mappend p1 p2) (mappend r1 r2)
    mappend ty@(Functional {}) st = mappend st ty
    mappend (Scalar s1) (Scalar s2) = Scalar (s1 `mappend` s2)
    mappend (Alpha _ s1) s2 = s2 `mappend` s1

data StructuralType = Attributes {
      type_name :: Maybe String
    , type_attributes :: Map String PyType
}

data FunctionType = FunctionType [StructuralType] StructuralType

instance Monoid StructuralType where
    mappend = unionType
    mempty = emptyType

intersectTypes :: PyType -> PyType -> PyType
intersectTypes = it
    where it Void _ = Void
          it _ Void = Void
          it Any x  = x
          it x Any  = x
          it (Scalar s1) (Scalar s2) = Scalar (structuralTypeIntersection s1 s2)
          it (Alpha s1 t1) (Scalar (Attributes s2 _)) | ((s1==) <$> s2) == Just True = Alpha s1 t1
          it (Alpha s1 t1) (Alpha s2 t2) | s1 == s2 = Alpha s1 t1
          it (Alpha _ t1) t2 = intersectTypes t1 t2
          it t2 (Alpha _ t1) = intersectTypes t1 t2
          it fn@(Functional {}) t2 = intersectTypes (Scalar $ singletonType "__call__" fn) t2
          it t1 fn@(Functional {}) = intersectTypes fn t1

(><) = intersectTypes

mkAlpha :: PyType -> PyType
mkAlpha s@(Scalar (Attributes name _)) = Alpha (fromMaybe "" name) s
mkAlpha s = Alpha "" s

getAttribute :: PyType -> String -> Maybe PyType
getAttribute (Scalar st) str = attributeType st str
getAttribute (Alpha _ st) str = getAttribute st str
getAttribute _ _ = Nothing

callType :: PyType -> Maybe ([PyType], PyType)
callType (Functional a b) = Just (map snd a, b)
callType (Scalar st) =
    case attributeType st "__call__" of
        Just (Functional a b) -> Just (map snd a, b)
        _ -> Nothing

data TypeError = Incompatible PyType PyType | Difference PyType PyType [[String]]

matchType :: PyType -> PyType -> Maybe TypeError
matchType t1 t2 | isCompatibleWith t1 t2 = Nothing
matchType t1@(Scalar s1) t2@(Scalar s2) =
    case missingAttributes s2 s1 of
        [] -> Nothing
        l -> Just (Difference t2 t1 l)
matchType t1 t2 = Just $ Incompatible t1 t2

anyType :: PyType
anyType = Any

structuralTypeIntersection :: StructuralType -> StructuralType -> StructuralType
structuralTypeIntersection (Attributes s1 m1) (Attributes s2 m2) =
    Attributes (if s1 == s2 then s1 else Nothing) $
        Map.intersectionWith intersectTypes m1 m2

typeDifference :: StructuralType -> StructuralType -> Map String PyType
typeDifference (Attributes _ s1) (Attributes _ s2) =  s1 `Map.difference` s2

typeToString :: FunctionType -> String
typeToString (FunctionType args ret) = intercalate " -> " $ map show (args ++ [ret])

setTypeName :: String -> StructuralType -> StructuralType
setTypeName str typ = typ {type_name = Just str}

getTypeName :: StructuralType -> String
getTypeName (Attributes Nothing _) = "?"
getTypeName (Attributes (Just s) _) = s

fromSet :: Set String -> StructuralType
fromSet = Attributes Nothing . Map.fromList . map (,anyType) . Set.toList

fromList :: [String] -> StructuralType
fromList = fromSet . Set.fromList

toMap :: StructuralType -> Map String PyType
toMap (Attributes _ s) = s

emptyType :: StructuralType
emptyType = Attributes Nothing Map.empty

singletonType :: String -> PyType -> StructuralType
singletonType str typ = Attributes Nothing $ Map.singleton str typ

attributeType :: StructuralType -> String -> Maybe PyType
attributeType (Attributes _ m) s = Map.lookup s m

addAttribute :: String -> PyType -> StructuralType -> StructuralType
addAttribute attr typ (Attributes n m) = Attributes n $ Map.insert attr typ m

addAllAttributes :: [(String, PyType)] -> StructuralType
addAllAttributes = mconcatMap (uncurry singletonType)

unionType :: StructuralType -> StructuralType -> StructuralType
unionType (Attributes n m1) (Attributes _ m2) = Attributes n (Map.unionWith mappend m1 m2)

typeHasAttr :: StructuralType -> String -> Bool
typeHasAttr (Attributes _ m) str = Map.member str m

isCompatibleWith :: PyType -> PyType -> Bool
isCompatibleWith Void _ = False
isCompatibleWith _ Void = False
isCompatibleWith Any _ = True
isCompatibleWith _ Any = True
isCompatibleWith (Scalar t1) (Scalar t2) = isCompatibleWithStr t2 t1
isCompatibleWith (Functional p1 r1) (Functional p2 r2) = and (zipWith isCompatibleWith (map snd p1) (map snd p2)) && isCompatibleWith r1 r2
isCompatibleWith (Alpha s1 _) (Alpha s2 _) = s1 == s2
isCompatibleWith (Alpha _ s1) s2 = isCompatibleWith s1 s2
isCompatibleWith _ _ = False

isCompatibleWithStr :: StructuralType -> StructuralType -> Bool
isCompatibleWithStr (Attributes _ s1) (Attributes _ s2) | Map.null s1 = True
                                                     | otherwise =
                                                        and $ for (Map.toList s2) $
                                                                \(member, typ1) ->
                                                                    {- Iterate down and make sure all the sub expressions are also
                                                                     - compatible with eachother. If the expression exists in s2, but
                                                                     - not in s1, then False is returned, otherwise the compatibility
                                                                     - of the other two types is returned -}
                                                                    maybe' (Map.lookup member s1) False $ \typ2 ->
                                                                        isCompatibleWith typ1 typ2

missingAttributes :: StructuralType -> StructuralType -> [[String]]
missingAttributes (Attributes _ s1) (Attributes _ s2) | Map.null s1 = []
                                                      | otherwise =
                                                         execWriter $
                                                            forM_ (Map.toList s2) $ \(member, typ1) ->
                                                                maybe' (Map.lookup member s1) (tell [[member]]) $ \typ2 ->
                                                                    case (typ1, typ2) of
                                                                        (Scalar t1', Scalar t2') ->
                                                                            tell (map (member:) $ missingAttributes t1' t2')
                                                                        _ -> return ()


instance Show StructuralType where
    show (Attributes name strs) =
        case Map.toList strs of
            [] -> "Any"
            l ->
                fromMaybe "" name ++ "{" ++ intercalate ", " (map (\(str, typ) -> str ++ " :: " ++ show typ) l) ++ "}"

instance Show FunctionType where
    show (FunctionType params ret) =
            intercalate " -> " $ map show (params ++ [ret])

data Function = Function
                 String -- name of function
                 FunctionType -- type of the function

toStructuralType :: HClass -> StructuralType
toStructuralType (HClass nm ty _) = setTypeName nm ty

data HClass = HClass {
                hclass_name :: String -- name of class
              , hclass_type :: StructuralType -- inferred structural type of class
              , hclass_methods :: Map String Function -- member functions
              }

{- Lift a type from being observed at the root to being observed
 - as the type of an attribute of some greater type. -}
liftType :: String -> PyType -> PyType
liftType str st = Scalar $ Attributes Nothing $ Map.singleton str st

liftFromDotList :: [String] -> PyType -> PyType
liftFromDotList list init = foldr liftType init list

typeFromDotList :: [String] -> PyType
typeFromDotList = foldr liftType anyType

prettyType :: PyType -> String
prettyType = prettyType' False

prettyType' :: Bool -> PyType -> String
prettyType' descend typ = execWriter $ prettyType' 0 typ
    where
          prettyType' indent (Scalar (Attributes (Just name) s)) = tell name >> when descend (tell " " >> prettyType' indent (Scalar (Attributes Nothing s)))
          prettyType' indent (Scalar (Attributes Nothing attrs)) = do
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

isVoid :: PyType -> Bool
isVoid Void = True
isVoid _ = False
