module Salo

import Salo.Language.Lexer
import Salo.Language.Parser

import Text.Lexer

main : IO ()
main = do putStrLn "Hello, world!"
          putStrLn "(You were probably looking for `nix run .#test`)"
