module Salo

import Salo.Compiler.Parser
import Text.Lexer

main : IO ()
main = putStrLn (show (Salo.Compiler.Parser.lex "       ->            ->  "))
