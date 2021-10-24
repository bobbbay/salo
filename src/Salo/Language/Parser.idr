module Salo.Language.Parser

import Salo.Language.Lexer
import Salo.Language.TT
import Salo.Language.AST

import Text.Lexer
import Text.Parser

Rule : Type -> Type
Rule = Grammar (TokenData Token) True

token : Token -> Rule ()
token t = terminal ("expecting " ++ show t) $ \t' =>
  if t == tok t'
    then Just ()
    else Nothing

kwd : String -> Rule ()
kwd s = token (Keyword s)

dataDecl : Rule Definition
dataDecl = do
  kwd "data"
  pure (MkDef (B "AAA" (P)) Postulate)

export
parse : String -> Either ParseError (Definition)
parse src = case lex src of
  Left err => Left err
  Right ts => case Text.Parser.Core.parse (dataDecl <* eof) ts of
    Left (Error msg []) => Left $ SyntaxError 0 0 msg []
    Left (Error msg (tt :: tts)) => Left $ SyntaxError (line tt) (col tt) msg (tt :: tts)
    Right (gs, []) => Right gs
    Right (gs, t::ts) => Left $ SyntaxError (line t) (col t) "eof expected" (t :: ts)

namespace Test
  namespace Examples
    export
    exampleData : List (TokenData Token)
    exampleData = [MkToken 0 0 (Keyword "data")]

  ParserType : Type
  ParserType = Either ParseError (Definition)

  parseData : ParserType
  parseData = parse "data"

  testHarness : ParserType -> IO ()
  testHarness t = case t of
    Left err => putStrLn $ "  -> Error: "   ++ (show err)
    Right gs => putStrLn $ "  -> Success: " ++ (show gs)

  export
  test : IO ()
  test = do putStrLn "Parser:"
            testHarness parseData
