module Salo

import Salo.Language.Lexer
import Salo.Language.Parser

import Text.Lexer

main : IO ()
main = do Salo.Language.Lexer.Test.test
          Salo.Language.Parser.Test.test
