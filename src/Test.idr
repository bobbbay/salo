module Test

import Salo.Language.Lexer
import Salo.Language.Parser

main : IO ()
main = do Salo.Language.Lexer.Test.test
          Salo.Language.Parser.Test.test
