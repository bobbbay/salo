module Test.Parser

import Hedgehog

-- This is currently an example test.

-- Hedgehog uses random generators for diverse tests.
exampleGenerator : Gen (List Char)
exampleGenerator = list (linear 0 30) alphaNum

-- For every example generator run, check that xs === ['a']
initialTest : Property
initialTest = property $ do xs <- forAll exampleGenerator
                            xs === ['a']

-- Same as the above.
initialTest2 : Property
initialTest2 = property $ do xs <- forAll exampleGenerator
                             xs === ['a']

-- We do the actual testing here.
-- Note that we use MkGroup to nicely group the two tests together.
export
test : IO Bool
test = checkGroup $ MkGroup "Parser" [
     ("initialTest", initialTest),
     ("initialTest2", initialTest2)
     ]
