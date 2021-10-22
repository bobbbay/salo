module Salo

import Salo.Language.Lexer
import Text.Lexer

main : IO ()
main = do Salo.Language.Lexer.Test.test
