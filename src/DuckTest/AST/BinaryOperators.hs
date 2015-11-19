{- This module contais a function that maps binary
 - operators to their respective 'dunder' names -}

module DuckTest.AST.BinaryOperators where

import Language.Python.Common

toDunderName :: Op a -> String
toDunderName op = case op of
    And _ -> "__bool__"
    Or  _ -> "__bool__"
    Not _ -> "__bool__"
    Exponent _ -> "__pow__"
    LessThan _ -> "__lt__"
    GreaterThan _ -> "__gt__"
    Equality _ -> "__eq__"
    GreaterThanEquals _ -> "__gte__"
    LessThanEquals _ -> "__gte__"
    NotEquals _ -> "__ne__"
    In _ -> "__contians__"
    Is _ -> "is"
    IsNot _ -> "is_not"
    NotIn _ -> "__contains__"
    BinaryOr _ -> "__or__"
    BinaryAnd _ -> "__and__"
    Xor _ -> "__xor__"
    ShiftLeft _ -> "__lshift__"
    ShiftRight _ -> "__rshift__"
    Multiply _ -> "__mul__"
    Plus _ -> "__add__"
    Minus _ -> "__sub__"
    Divide _ -> "__div__"
    FloorDivide _ -> "__floordiv__"
    Invert _ -> "__inv__"
    Modulo _ -> "__mod__"

    _ -> ""
