{
module Salo.Language.Parser ( parseExpr, parseTokens ) where

import Salo.Language.Lexer
import Salo.Language.Syntax

import Control.Monad.Except
}

%tokentype { Token }

%token
  module   { TokenModule }
  true     { TokenTrue }
  false    { TokenFalse }
  ':'      { TokenColon }
  '|'      { TokenBar }
  '->'     { TokenArrow }
  '='      { TokenEq }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  '+'      { TokenAdd }
  '-'      { TokenSub }
  '*'      { TokenMul }
  '/'      { TokenDiv }
  '\n'     { TokenNewline }
  NUM      { TokenNum $$ }
  IDENT    { TokenIdent $$ }

%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name salo

-- Operators
%left '+' '-'
%left '*' '/'
%%

Salo : Expr '\n'         { Expr $1 }
     | module IDENT '\n' { Module $2 }

Expr : IDENT ':' IDENT { Decl $1 $3 }
     | IDENT '=' Expr  { Def $1 $3 }
     | Literal         { Lit $1 }
     | Expr Op Expr    { Op $1 $2 $3 }

Literal : NUM     { LInt $1 }
        | true    { LBool True }
        | false   { LBool False }
        | '('')'  { LUnit }

Op : '+' { Plus }
   | '-' { Minus }
   | '*' { Multiply }
   | '/' { Divide }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Salo
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  salo tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
