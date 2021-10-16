module Salo

import Salo.Language.Lexer
import Text.Lexer

main : IO ()
main = putStrLn (show (Salo.Language.Lexer.lex """
  main -> x -> y

  -- This is a comment.

  Type -> Type

  data x -> x
  """))
