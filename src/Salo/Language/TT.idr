module Salo.Language.TT

public export
data TT : Type -> Nat -> Type where
  P : TT m n

public export
record Binding (n : Nat) where
  constructor B
  name : String
  type : TT Bool n
