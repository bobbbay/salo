module Salo.Language.Test ( tests ) where

import Distribution.TestSuite

import Salo.Language.Lexer ( scanner )
import Salo.Language.Parser ( parse )

tests :: IO [Test]
tests = return [ {- There are no tests! -}]
