module Salo.Compiler.Parser

import Text.Lexer
import Data.List

public export
data Token
  = Arrow

  | Space
  | Comment

public export
Show Token where
  show Arrow = "->"
  show Space = "(space)"
  show Comment = "(comment)"

public export
Eq Token where
  (==) x y = case (x, y) of
    (Arrow, Arrow)     => True
    (Space, Space)     => True
    (Comment, Comment) => True
    _                  => False

public export
data ParseError : Type where
  LexError    : Int -> Int -> String -> ParseError
  --            Line,  Col,   Message
  SyntaxError : Int -> Int -> String -> List (TokenData Token) -> ParseError

public export
Show ParseError where
  show (LexError l c msg)       = "Lex error at " ++ show (l, c) ++ ": " ++ msg
  show (SyntaxError l c msg ts) = "Parse error at " ++ show (l, c) ++ ": " ++ msg ++ "; next up: " ++ show [tok t | t <- take 8 ts]

export
lex : String -> Either ParseError (List (TokenData Token))
lex src = case lex tokens src of
            (ts, (_, _, "")) => Right $ filter notSpace ts
            (_, (l, c, rest)) => Left $ LexError l c rest
            _ => Left $ LexError 1 1 ""
          where
            notSpace : TokenData Token -> Bool
            notSpace td = case tok td of
              Space   => False
              Comment => False
              _       => True

            ident : Lexer
            ident = some $ pred (\x => isAlpha x || isDigit x || x == '_' || x == '\'')

            tokens : TokenMap Token
            tokens =
              [ (exact "->",                const Arrow)

              , (space,                     const Space)
              , (lineComment (exact "--"),  const Comment)
            ]
