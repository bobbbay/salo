{

module Salo.Language.Parser ( parseExpr ) where

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
  NUM      { TokenNum $$ }
  IDENT    { TokenIdent $$ }

%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name expr

-- Operators
%left '+' '-'
%left '*' '/'
%%

Expr : IDENT ':' IDENT { Decl $1 $3 }
     | IDENT '=' Expr  { Def $1 $3 }
     | Literal         { Lit $1 }
     | Expr Op Expr    { Op $1 $2 $3 }

Literal : NUM     { LInt $1 }
        | true    { LBool True }
        | false   { LBool False }

Op : '+' { Plus }
   | '-' { Minus }
   | '*' { Multiply }
   | '/' { Divide }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
}
