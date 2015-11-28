module DuckTest.Builtins where

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map
import DuckTest.Types

builtinGlobalFunctions :: Map String Function
builtinGlobalFunctions =
    Map.fromList $ map (\f@(Function name _) -> (name, f))[
          Function "print" (FunctionType [emptyType] emptyType)
        , Function "len" (FunctionType [fromList ["__len__"]] emptyType)
        , Function "str" (FunctionType [fromList ["__str__"]] (hclass_type strClass))
        , Function "open" (FunctionType [emptyType] emptyType)
        , Function "int" (FunctionType [emptyType] emptyType)
    ]

builtinGlobalClasses :: Map String HClass
builtinGlobalClasses =
    Map.fromList $
        map (\c@(HClass name _ _) -> (name, c)) [
            strClass
        ]

strClass :: HClass
strClass = HClass "str" (fromList stringAttrs) Map.empty

stringAttrs :: [String]
stringAttrs = ["__add__", "__class__", "__contains__",
               "__delattr__", "__dir__", "__doc__",
               "__eq__", "__format__", "__ge__",
               "__getattribute__", "__getitem__", "__getnewargs__",
               "__gt__", "__hash__", "__init__",
               "__iter__", "__le__", "__len__",
               "__lt__", "__mod__", "__mul__",
               "__ne__", "__new__", "__reduce__",
               "__reduce_ex__", "__repr__", "__rmod__",
               "__rmul__", "__setattr__", "__sizeof__",
               "__str__", "__subclasshook__", "capitalize",
               "casefold", "center", "count",
               "encode", "endswith", "expandtabs",
               "find", "format", "format_map",
               "index", "isalnum", "isalpha",
               "isdecimal", "isdigit", "isidentifier",
               "islower", "isnumeric", "isprintable",
               "isspace", "istitle", "isupper",
               "join", "ljust", "lower",
               "lstrip", "maketrans", "partition",
               "replace", "rfind", "rindex", "rjust",
               "rpartition", "rsplit", "rstrip", "split",
               "splitlines", "startswith", "strip", "swapcase",
               "title", "translate", "upper", "zfill"]
