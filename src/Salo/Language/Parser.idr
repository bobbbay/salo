module Salo.Language.Parser

import Salo.Language.Lexer
import Salo.Language.TT
import Salo.Language.AST

import Text.Lexer
import Text.Parser

Rule : Type -> Type
Rule = Grammar (TokenData Token) True

token : Token -> Rule ()
token t = terminal ("expecting " ++ show t) $ \t' =>
  if t == tok t'
    then Just ()
    else Nothing

kwd : String -> Rule ()
kwd s = token (Keyword s)

dataDecl : Rule Definition
dataDecl = do
  kwd "data"
  pure (MkDef (B "AAA" (P)) Postulate)

namespace Test
  namespace Examples
    export
    exampleData : List (TokenData Token)
    exampleData = [MkToken 0 0 (Keyword "data")]

  -- TODO: Complete tests with `parse`
