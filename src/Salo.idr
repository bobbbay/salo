module Salo

import Salo.Language.Lexer
import Text.Lexer

main : IO ()
main = putStrLn (show (Salo.Language.Lexer.lex "       ->            ->  "))
