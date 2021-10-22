module Salo.Language.Lexer

import Text.Lexer
import Data.List
import Data.String

||| All possible tokens.
public export
data Token
  = Arrow
  | Colon
  | Equals

  | Space
  | Comment

  | Keyword String
  | Ident String
  | StringLit String

public export
Show Token where
  show Arrow       = "->"
  show Colon       = ":"
  show Equals      = "="

  show Space       = "(space)"
  show Comment     = "(comment)"

  show (Keyword s) = "keyword " ++ show s
  show (Ident s)   = "identifier " ++ show s
  show (StringLit s) = show s

public export
Eq Token where
  (==) x y = case (x, y) of
    (Arrow, Arrow)     => True
    (Colon, Colon)     => True
    (Equals, Equals)   => True

    (Space,   Space)   => True
    (Comment, Comment) => True

    (Keyword s,   Keyword s')   => s == s'
    (Ident s,     Ident s')     => s == s'
    (StringLit s, StringLit s') => s == s'

    _                  => False

||| All possible errors that the parser can encounter.
public export
data ParseError : Type where
  --            Line,  Col,   Rest
  LexError    : Int -> Int -> String -> ParseError
  --            Line,  Col,   Message,  Rest
  SyntaxError : Int -> Int -> String -> List (TokenData Token) -> ParseError

public export
Show ParseError where
  show (LexError l c msg)       = "Lex error at " ++ show (l, c) ++ ": " ++ msg
  show (SyntaxError l c msg ts) = "Parse error at " ++ show (l, c) ++ ": " ++ msg ++ "; next up: " ++ show [tok t | t <- take 8 ts]

||| Lexes a given string.
|||
|||  @ src String to parse.
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

            parseString : String -> Token
            parseString s = StringLit $ assert_total (strTail (reverse (strTail (reverse s))))

            ||| Given a string, check if it's a keyword from the list of
            ||| keywords, or an identifier.
            kwdOrIdent : String -> Token
            kwdOrIdent s =
              if s `elem` keywords
                then Keyword s
                else Ident   s
              where
                keywords : List String
                keywords =
                  [ "Type"
                  , "data"
                  ]

            tokens : TokenMap Token
            tokens =
              [ (exact "->",                const Arrow)
              , (exact ":",                 const Colon)
              , (exact "=",                 const Equals)

              , (space,                     const Space)
              , (lineComment (exact "--"),  const Comment)

              , (ident,                     kwdOrIdent)
              , (stringLit,                 parseString)
            ]
