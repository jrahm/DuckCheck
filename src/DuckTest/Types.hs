{-# LANGUAGE TupleSections #-}
module DuckTest.Types where

import DuckTest.Internal.Common

import qualified Data.Set as Set
import qualified Data.Map as Map

data StructuralType = Attributes {
      type_name :: Maybe String
    , type_attributes :: Map String StructuralType
}

instance Monoid StructuralType where

    mappend = unionType

    mempty = emptyType

typeDifference :: StructuralType -> StructuralType -> Map String StructuralType
typeDifference (Attributes _ s1) (Attributes _ s2) =  s1 `Map.difference` s2

typeToString :: FunctionType -> String
typeToString (FunctionType args ret) = intercalate " -> " $ map show (args ++ [ret])

setTypeName :: String -> StructuralType -> StructuralType
setTypeName str typ = typ {type_name = Just str}

getTypeName :: StructuralType -> String
getTypeName (Attributes Nothing _) = "?"
getTypeName (Attributes (Just s) _) = s


fromSet :: Set String -> StructuralType
fromSet = Attributes Nothing . Map.fromList . map (,emptyType) . Set.toList

fromList :: [String] -> StructuralType
fromList = fromSet . Set.fromList

toMap :: StructuralType -> Map String StructuralType
toMap (Attributes _ s) = s

emptyType :: StructuralType
emptyType = Attributes Nothing Map.empty

singletonType :: String -> StructuralType
singletonType = Attributes Nothing . flip Map.singleton emptyType

addAttribute :: String -> StructuralType -> StructuralType -> StructuralType
addAttribute attr typ (Attributes n m) = Attributes n $ Map.insert attr typ m

unionType :: StructuralType -> StructuralType -> StructuralType
unionType (Attributes n m1) (Attributes _ m2) = Attributes n (Map.unionWith mappend m1 m2)

typeHasAttr :: StructuralType -> String -> Bool
typeHasAttr (Attributes _ m) str = Map.member str m

isCompatibleWith :: StructuralType -> StructuralType -> Bool
isCompatibleWith (Attributes _ s1) (Attributes _ s2) | Map.null s1 = True
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

data FunctionType = FunctionType [StructuralType] StructuralType
instance Show FunctionType where
    show (FunctionType params ret) =
            intercalate "->" $ map show (params ++ [ret])

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
liftType :: String -> StructuralType -> StructuralType
liftType str st = Attributes Nothing $ Map.singleton str st
