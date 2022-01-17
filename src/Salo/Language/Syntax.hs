module Salo.Language.Syntax where

type Name = String
type Type = Name

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
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Op
  = Plus
  | Minus
  | Multiply
  | Divide
  deriving (Show, Eq)
