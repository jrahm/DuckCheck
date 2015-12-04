{-# LANGUAGE TupleSections #-}
module DuckTest.Builtins where

import DuckTest.Internal.State
import DuckTest.Types
import Text.Printf

initState :: InternalState
initState =
    addVariableType "print" (Functional [("", Any)] Any) $
    addVariableType "len" (Functional [("", fromList Nothing $ map (,Any) ["__len__"])] Any) $
    addVariableType "str" (Functional [("", fromList Nothing $ map (,Any) ["__str__"])] strType) $
    addVariableType "int" (Functional [("", Any)] intType) $
    addVariableType "set" (Functional [] setType) $
    addVariableType "list" (Functional [] $ listType Any)
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

sysType :: PyType
sysType = fromList Nothing sysAttrs

strType :: PyType
strType = fromList (Just "str") stringAttrs

intType :: PyType
intType = fromList (Just "int") intAttrs

setType :: PyType
setType = fromList (Just "set") setAttrs

listType :: PyType -> PyType
listType _T = fromList (Just $ printf "list<%s>" (prettyType _T))  (listAttrs _T)

setAttrs :: [(String, PyType)]
setAttrs =
    map (,Any)
        ["__and__", "__class__", "__contains__", "__delattr__",
        "__dir__", "__doc__", "__eq__", "__format__",
        "__ge__", "__getattribute__", "__gt__", "__hash__",
        "__iand__", "__init__", "__ior__", "__isub__",
        "__iter__", "__ixor__", "__le__", "__len__",
        "__lt__", "__ne__", "__new__", "__or__",
        "__rand__", "__reduce__", "__reduce_ex__", "__repr__",
        "__ror__", "__rsub__", "__rxor__", "__setattr__",
        "__sizeof__", "__str__", "__sub__", "__subclasshook__",
        "__xor__", "add", "clear", "copy",
        "difference", "difference_update", "discard", "intersection",
        "intersection_update", "isdisjoint", "issubset", "issuperset",
        "pop", "remove", "symmetric_difference", "symmetric_difference_update",
        "union", "update"]

intAttrs :: [(String, PyType)]
intAttrs =
    map (,Any)
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
         "imag", "numerator", "real", "to_bytes", "__div__"]
    `mappend`
        [("__add__", Functional [("", mkAlpha intType)] (mkAlpha intType)),
         ("__str__", Functional [] strType)]

stringAttrs :: [(String, PyType)]
stringAttrs =
    map (,Any)
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

sysAttrs :: [(String, PyType)]
sysAttrs =
    map (,Any)
    ["__displayhook__", "__doc__", "__excepthook__", "__interactivehook__",
    "__loader__", "__name__", "__package__", "__spec__",
    "__stderr__", "__stdin__", "__stdout__", "_clear_type_cache",
    "_current_frames", "_debugmallocstats", "_getframe", "_home",
    "_mercurial", "_xoptions", "abiflags", "api_version",
    "argv", "base_exec_prefix", "base_prefix", "builtin_module_names",
    "byteorder", "call_tracing", "callstats", "copyright",
    "displayhook", "dont_write_bytecode", "exc_info", "excepthook",
    "exec_prefix", "executable", "exit", "flags",
    "float_info", "float_repr_style", "get_coroutine_wrapper", "getallocatedblocks",
    "getcheckinterval", "getdefaultencoding", "getdlopenflags", "getfilesystemencoding",
    "getprofile", "getrecursionlimit", "getrefcount", "getsizeof",
    "getswitchinterval", "gettrace", "hash_info", "hexversion",
    "implementation", "int_info", "intern", "is_finalizing",
    "maxsize", "maxunicode", "meta_path", "modules",
    "path", "path_hooks", "path_importer_cache", "platform",
    "prefix", "set_coroutine_wrapper", "setcheckinterval", "setdlopenflags",
    "setprofile", "setrecursionlimit", "setswitchinterval", "settrace",
    "stderr", "stdin", "stdout", "thread_info",
    "version", "version_info", "warnoptions"]
    `mappend`
        [("platform", strType),
         ("argv", listType strType)]

listAttrs :: PyType -> [(String, PyType)]
listAttrs _T =
    map (,Any)
        ["__add__", "__class__", "__contains__", "__delattr__",
        "__delitem__", "__dir__", "__doc__", "__eq__",
        "__format__", "__ge__", "__getattribute__", "__getitem__",
        "__gt__", "__hash__", "__iadd__", "__imul__",
        "__init__", "__iter__", "__le__", "__len__",
        "__lt__", "__mul__", "__ne__", "__new__",
        "__reduce__", "__reduce_ex__", "__repr__", "__reversed__",
        "__rmul__", "__setattr__", "__setitem__", "__sizeof__",
        "__str__", "__subclasshook__", "append", "clear",
        "copy", "count", "extend", "index",
        "insert", "pop", "remove", "reverse",
        "sort"]
    `mappend`
        [ ("__add__", Functional [("arg0", mkAlpha $ listType _T)] (mkAlpha $ listType _T))
        , ("append", Functional [("arg0", _T)] Void)
        , ("__iter__", Functional [] (basicIteratorType _T))
        ]

basicIteratorType :: PyType -> PyType
basicIteratorType _T = fromList Nothing [("__next__", Functional [] _T)]
