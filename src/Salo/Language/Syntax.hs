module Salo.Language.Syntax where

type Name = String
type Type = Name

data Salo
  =
    -- | A module declaration
    Module Name
    -- | An expression
  | Expr Expr
  deriving (Show, Eq)

data Expr
  =
    -- | A value type declaration
    Decl Name Type
    -- | A value definition
  | Def Name Expr
    -- | A function application
  | App Expr Expr
    -- | Operator application
  | Op Expr Op Expr
    -- | A literal value
  | Lit Lit
  deriving (Show, Eq)

data Lit
  =
    -- | A boolean type, based on Haskell's Bool
    LitBool Bool
    -- | An integer, based off of Haskell's Int
  | LitInt Int
    -- | A string, based off of Haskell's String
  | LitString String
    -- | A unit type ()
  | LitUnit
  deriving (Show, Eq, Ord)

data Op
  = Plus
  | Minus
  | Multiply
  | Divide
  deriving (Show, Eq)
