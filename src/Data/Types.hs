module Data.Types where

data Literal =
    Bool Bool
  | Int Integer
  | Char Char
  deriving (Show, Eq)
