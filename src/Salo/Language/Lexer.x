{
module Salo.Language.Lexer (
  Token(..),
  scanTokens
) where

import Data.Text (Text)
import Data.Text qualified as T
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$misc  = [\_]
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
  module                        { \s -> TokenModule }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }

  "->"                          { \s -> TokenArrow }
  "="                           { \s -> TokenEq }
  ":"                           { \s -> TokenColon }
  "("                           { \s -> TokenLParen }
  ")"                           { \s -> TokenRParen }
  "|"                           { \s -> TokenBar }

  $digit+                       { \s -> TokenNum (read s) }
  -- $alpha [$alpha $digit \_]*    { \s -> TokenIdent s }

{
data Token 
  = TokenModule
  | TokenBar
  | TokenTrue
  | TokenFalse
  | TokenColon
  | TokenNum Int
  | TokenIdent String
  | TokenArrow
  | TokenEq
  | TokenLParen
  | TokenRParen
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
