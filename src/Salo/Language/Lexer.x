{
module Salo.Language.Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except
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
  true                          { \s -> TokenTrue }
  false                         { \s -> TokenFalse }

  ":"                           { \s -> TokenColon }
  "|"                           { \s -> TokenBar }
  "->"                          { \s -> TokenArrow }
  "="                           { \s -> TokenEq }
  "("                           { \s -> TokenLParen }
  ")"                           { \s -> TokenRParen }
  "+"                           { \s -> TokenAdd }
  "-"                           { \s -> TokenSub }
  "*"                           { \s -> TokenMul }
  "/"                           { \s -> TokenDiv }

  $digit+                       { \s -> TokenNum (read s) }
  $alpha [$alpha $digit \_]*    { \s -> TokenIdent s }

{
data Token 
  = TokenModule       -- module
  | TokenTrue         -- true
  | TokenFalse        -- false
  | TokenColon        -- :
  | TokenBar          -- \|
  | TokenArrow        -- ->
  | TokenEq           -- =
  | TokenLParen       -- (
  | TokenRParen       -- )
  | TokenAdd          -- +
  | TokenSub          -- -
  | TokenMul          -- \*
  | TokenDiv          -- /
  | TokenEOF          -- EOF
  | TokenNum Int      -- 1, 2, 3
  | TokenIdent String -- "A", "B", "C"
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError x -> throwError "Invalid lexeme. "
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)
}
