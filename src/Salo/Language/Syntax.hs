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
    -- | A literal value
  | Lit Lit
  deriving (Eq,Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)
