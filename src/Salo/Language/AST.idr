module Salo.Language.AST

import Salo.Language.TT

public export
data Body : Type where
  Postulate : Body

public export
record Definition where
  constructor MkDef
  binding : Binding Z
  body : Body
