{
module Salo.Language.Parser ( parse ) where

import Salo.Language.Lexer
import Salo.Language.Syntax
import Control.Monad.Except
}

%tokentype { Lexeme }

%token
  -- Literals
  BOOL     { L _ (LBool $$) _ }

  INT      { L _ (LInt $$) _ }
  STRING   { L _ (LString $$) _ }

  -- Reserved keywords
  module   { L _ (LModule) _ }
  '+'      { L _ (LPlus) _ }
  '-'      { L _ (LMinus) _ }
  '*'      { L _ (LMultiply) _ }
  '/'      { L _ (LDivide) _ }

  -- Symbols
  '('      { L _ (LLParen) _ }
  ')'      { L _ (LRParen) _ }
  ':'      { L _ (LColon) _ }
  '='      { L _ (LEqual) _ }
  '|'      { L _ (LBar) _ }
  '->'     { L _ (LArrow) _ }

  -- Identifiers
  IDENT    { L _ (LIdentifier $$) _ }

%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Entry point
%name salo

-- Operators
%left '+' '-'
%left '*' '/'
%%

Salo : Expr            { Expr $1 }
     | module IDENT    { Module $2 }

Expr : IDENT ':' IDENT { Decl $1 $3 }
     | IDENT '=' Expr  { Def $1 $3 }
     | Literal         { Lit $1 }
     | Expr Op Expr    { Op $1 $2 $3 }

Literal : INT          { LitInt $1 }
        | BOOL         { LitBool $1 }
        | STRING       { LitString $1 }
        | '('')'       { LitUnit }

Op : '+'               { Plus }
   | '-'               { Minus }
   | '*'               { Multiply }
   | '/'               { Divide }

{
parseError :: [Lexeme]  -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of input."

parse = runExcept . salo
}
