{-# LANGUAGE TupleSections #-}
module DuckTest.Types where

import DuckTest.Internal.Common

import qualified Data.Set as Set
import qualified Data.Map as Map

data PyType = Scalar StructuralType | Functional [PyType] PyType | Any

instance Show PyType where
    show (Scalar st) = show st
    show (Functional args ret) = "(" ++ intercalate "," (map show args) ++ ") -> " ++ show ret

instance Monoid PyType where
    mempty = Any

    mappend Any x = x
    mappend x Any = x
    mappend (Scalar st) ty@(Functional {}) =
        {- In Python, if we observe a variable being used as a function and a scalar,
         - then that variable is a scalar with a call function -}
        Scalar (addAttribute "__call__" ty st)
    mappend (Functional p1 r1) (Functional p2 r2) = Functional (zipWith mappend p1 p2) (mappend r1 r2)
    mappend ty@(Functional {}) st = mappend st ty
    mappend (Scalar s1) (Scalar s2) = Scalar (s1 `mappend` s2)

data StructuralType = Attributes {
      type_name :: Maybe String
    , type_attributes :: Map String PyType
}

data FunctionType = FunctionType [StructuralType] StructuralType

instance Monoid StructuralType where

    mappend = unionType

    mempty = emptyType

getAttribute :: PyType -> String -> Maybe PyType
getAttribute (Scalar st) str = attributeType st str
getAttribute _ _ = Nothing

callType :: PyType -> Maybe ([PyType], PyType)
callType (Functional a b) = Just (a, b)
callType (Scalar st) =
    case attributeType st "__call__" of
        Just (Functional a b) -> Just (a, b)
        _ -> Nothing

data TypeError = Incompatible PyType PyType | Difference String (Map String PyType)

matchType :: PyType -> PyType -> Maybe TypeError
matchType t1 t2 | isCompatibleWith t1 t2 = Nothing
matchType (Scalar s1) (Scalar s2) = Just (Difference (getTypeName s1) $ typeDifference s2 s1)
matchType t1 t2 = Just $ Incompatible t1 t2

anyType :: PyType
anyType = Any

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

singletonType :: String -> StructuralType
singletonType = Attributes Nothing . flip Map.singleton anyType

attributeType :: StructuralType -> String -> Maybe PyType
attributeType (Attributes _ m) s = Map.lookup s m

addAttribute :: String -> PyType -> StructuralType -> StructuralType
addAttribute attr typ (Attributes n m) = Attributes n $ Map.insert attr typ m

unionType :: StructuralType -> StructuralType -> StructuralType
unionType (Attributes n m1) (Attributes _ m2) = Attributes n (Map.unionWith mappend m1 m2)

typeHasAttr :: StructuralType -> String -> Bool
typeHasAttr (Attributes _ m) str = Map.member str m

isCompatibleWith :: PyType -> PyType -> Bool
isCompatibleWith Any _ = True
isCompatibleWith _ Any = True
isCompatibleWith (Scalar t1) (Scalar t2) = isCompatibleWithStr t1 t2
isCompatibleWith (Functional p1 r1) (Functional p2 r2) = and (zipWith isCompatibleWith p1 p2) && isCompatibleWith r1 r2
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

typeFromDotList :: [String] -> PyType
typeFromDotList = foldr liftType anyType
