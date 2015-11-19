module Hiss.Types where

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

data StructuralType = Attributes {
      type_name :: Maybe String
    , type_attributes :: Set String
}

instance Monoid StructuralType where

    mappend = unionType

    mempty = emptyType

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

toStructuralType :: HClass -> StructuralType
toStructuralType (HClass nm ty _) = setTypeName nm ty

data HClass = HClass {
                hclass_name :: String -- name of class
              , hclass_type :: StructuralType -- inferred structural type of class
              , hclass_methods :: Map String Function -- member functions
              }

