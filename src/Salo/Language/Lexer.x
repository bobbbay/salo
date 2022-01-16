{
module Salo.Language.Lexer (
  Token(..),
  scanTokens
) where

-- import Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "--".*                         ;
  "---".*                        ;
  "{-".*"-}"                     ;
  "{--".*"--}"                   ;

  -- Syntax
  -- > Booleans
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }

  -- > Misc
  "->"                          { \s -> TokenArrow }
  "="                           { \s -> TokenEq }
  ":"                           { \s -> TokenColon }
  "("                           { \s -> TokenLParen }
  ")"                           { \s -> TokenRParen }
  "|"                           { \s -> TokenBar }

  -- > Literals
  $digit+                       { \s -> TokenNum (read s) }

{
data Token 
  = TokenBar
  | TokenTrue
  | TokenFalse
  | TokenColon
  | TokenNum Int
  | TokenArrow
  | TokenEq
  | TokenLParen
  | TokenRParen
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
