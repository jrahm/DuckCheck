{-# LANGUAGE TupleSections #-}
module DuckTest.Builtins where

import DuckTest.Internal.State
import DuckTest.Types

initState :: InternalState
initState =
    addVariableType "print" (Functional [("", anyType)] anyType) $
    addVariableType "len" (Functional [("", Scalar $ fromList ["__len__"])] anyType) $
    addVariableType "str" (Functional [("", Scalar $ fromList ["__str__"])] strType) $
    addVariableType "int" (Functional [("", anyType)] intType)
    emptyState
--
-- builtinGlobalFunctions :: Map String Function
-- builtinGlobalFunctions =
--     Map.fromList $ map (\f@(Function name _) -> (name, f))[
--           Function "print" (FunctionType [emptyType] emptyType)
--         , Function "len" (FunctionType [fromList ["__len__"]] emptyType)
--         , Function "str" (FunctionType [fromList ["__str__"]] (hclass_type strClass))
--         , Function "open" (FunctionType [emptyType] emptyType)
--         , Function "int" (FunctionType [emptyType] emptyType)
--     ]
--
-- builtinGlobalClasses :: Map String HClass
-- builtinGlobalClasses =
--     Map.fromList $
--         map (\c@(HClass name _ _) -> (name, c)) [
--             strClass
--         ]
--
-- strClass :: HClass
-- strClass = HClass "str" (fromList stringAttrs) Map.empty

strType :: PyType
strType = Scalar $ setTypeName "str" $ addAllAttributes stringAttrs

intType :: PyType
intType = Scalar $ setTypeName "int" $ addAllAttributes intAttrs

intAttrs :: [(String, PyType)]
intAttrs =
    map (,anyType)
        ["__abs__", "__add__", "__and__", "__bool__",
         "__ceil__", "__class__", "__delattr__", "__dir__", "__divmod__",
         "__doc__", "__eq__", "__float__", "__floor__",
         "__floordiv__", "__format__", "__ge__", "__getattribute__",
         "__getnewargs__", "__gt__", "__hash__", "__index__",
         "__init__", "__int__", "__invert__", "__le__",
         "__lshift__", "__lt__", "__mod__", "__mul__",
         "__ne__", "__neg__", "__new__", "__or__",
         "__pos__", "__pow__", "__radd__", "__rand__",
         "__rdivmod__", "__reduce__", "__reduce_ex__", "__repr__",
         "__rfloordiv__", "__rlshift__", "__rmod__", "__rmul__",
         "__ror__", "__round__", "__rpow__", "__rrshift__",
         "__rshift__", "__rsub__", "__rtruediv__", "__rxor__",
         "__setattr__", "__sizeof__", "__str__", "__sub__",
         "__subclasshook__", "__truediv__", "__trunc__", "__xor__",
         "bit_length", "conjugate", "denominator", "from_bytes",
         "imag", "numerator", "real", "to_bytes"]
    `mappend`
        [("__add__", Functional [("", mkAlpha intType)] (mkAlpha intType)),
         ("__str__", Functional [] strType)]

stringAttrs :: [(String, PyType)]
stringAttrs =
    map (,anyType)
        ["__add__", "__class__", "__contains__",
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
    `mappend`
        [("__add__", Functional [("", mkAlpha strType)] (mkAlpha strType))]
