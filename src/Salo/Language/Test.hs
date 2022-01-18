module Salo.Language.Test ( tests ) where

import Distribution.TestSuite

import Salo.Language.Lexer ( scanTokens )
import Salo.Language.Parser ( parseExpr )

tests :: IO [Test]
tests = return [ Test parseBasicExample
               ]

parseBasicExample = TestInstance
        { run = do
                   case parseExpr "1 + 1" of
                     Left e -> return $ Finished $ Fail $ "Received: " ++ e
                     Right a -> do
                       putStrLn $ "Received: " ++ (show a)
                       return $ Finished Pass
        , name = "Parse basic example"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right parseBasicExample
        }
