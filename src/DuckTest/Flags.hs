module DuckTest.Flags where

data LogLevel = Trace | Debug | Info | Warn | Error deriving (Eq, Ord, Enum, Show)

data Flag
      = Verbose LogLevel |
        Version2
        deriving (Show, Ord, Eq)
