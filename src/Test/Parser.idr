module Test.Parser

import Hedgehog

exampleGenerator : Gen (List Char)
exampleGenerator = list (linear 0 30) alphaNum

initialTest : Property
initialTest = property $ do xs <- forAll exampleGenerator
                            xs === xs

export
test : IO Bool
test = checkGroup $ MkGroup "Parser" [
     ("initialTest", initialTest)
     ]
