module Salo.Language.Test ( tests ) where

import Distribution.TestSuite

import Salo.Language.Lexer ( scanTokens )

tests :: IO [Test]
tests = return [ Test lexBasicExample ]

lexBasicExample = TestInstance
        { run = do
                   putStrLn $ show $ scanTokens "module"
                   return $ Finished Pass
        , name = "Lex basic example"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right lexBasicExample
        }
