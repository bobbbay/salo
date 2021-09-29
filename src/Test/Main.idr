module Test.Main

import Test.Parser

main : IO Bool
main = do Test.Parser.test
