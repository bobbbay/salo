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

public export
implementation Show Body where
  show Postulate = "Postulate"

public export
implementation Show Definition where
  show x = "Binding: " ++ show x.binding
        ++ " Body: "    ++ show x.body
