module Salo.Language.TT

public export
data TT : Type -> Nat -> Type where
  P : TT m n

public export
record Binding (n : Nat) where
  constructor B
  name : String
  type : TT Bool n

public export
implementation Show (Binding n) where
  show x = show x.name

public export
data Name = UN String

public export
implementation Show Name where
  show (UN s) = show s
