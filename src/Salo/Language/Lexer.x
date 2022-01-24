{
module Salo.Language.Lexer ( scanner, Lexeme(..), LexemeClass(..) ) where

import Salo
import Control.Monad.Except
import Numeric ( readDec )
import Data.Map ( Map )
import Data.Maybe
import qualified Data.Map as Map ( empty )
import Data.Char ( chr )
}

%wrapper "monadUserState"

$whitespace = [\ \t \b\n]

$digit = 0-9
@number = [$digit]+

$alpha = [A-Za-z]
@identifier = $alpha($alpha|.|$digit)*

state :-

-- Literals: booleans, integers, strings
<0>             "true" | "false"      { getBoolean }
<0>             @number               { getInteger }
<0>             \"                    { enterNewString `andBegin` state_string }
<state_string>  \\n                   { addCharToString '\n' }
<state_string>  \\t                   { addCharToString '\t' }
<state_string>  \\\^[@-_]             { addControlToString }
<state_string>  \\$digit$digit$digit  { addAsciiToString }
<state_string>  \\\"                  { addCharToString '\"' }
<state_string>  \\\\                  { addCharToString '\\' }
<state_string>  \\[\ \n\t\f\r\b\v]+\\ ;
<state_string>  \\                    { \_ _ -> lexerError "Illegal escape sequence" }
<state_string>  \"                    { leaveString `andBegin` state_initial }
<state_string>  .                     { addCurrentToString }
<state_string>  \n                    { skip }

-- Reserved keywords
<0>             "module"              { mkL LModule }
<0>             \+                    { mkL LPlus }
<0>             \-                    { mkL LMinus }
<0>             \*                    { mkL LMultiply }
<0>             \/                    { mkL LDivide }

-- Symbols
<0>             \(                    { mkL LLParen }
<0>             \)                    { mkL LRParen }
<0>             \:                    { mkL LColon }
<0>             \=                    { mkL LEqual }
<0>             \|                    { mkL LBar }
<0>             "->"                  { mkL LArrow }

-- Whitespace
<0>             $whitespace+          ;

-- Comments
<0>              "---".*              { skip }
<0>              "--".*               { skip }
<0>              "{-"                 { enterNewComment `andBegin` state_comment }
<state_comment>  "{-"                 { embedComment }
<state_comment>  "-}"                 { unembedComment }
<state_comment>  .                    ;
<state_comment>  \n                   { skip }

-- Identifiers
<0>             @identifier           { getVariable }
 
{
data Lexeme = L AlexPosn LexemeClass (Maybe String) deriving Show

data LexemeClass
  = LEOF
  -- Literals
  | LBool       Bool
  | LInt        Int
  | LString     String

  -- Reserved keywords
  | LModule
  | LPlus
  | LMinus
  | LMultiply
  | LDivide

  -- Symbols
  | LLParen
  | LRParen
  | LColon
  | LEqual
  | LBar
  | LArrow

  -- Identifiers
  | LIdentifier String
  deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = return (L p c (Just (take len str)))

-- States

state_initial :: Int
state_initial = 0

-- Actions

type Action = AlexInput -> Int -> Alex Lexeme

enterNewComment, embedComment, unembedComment :: Action
enterNewString, leaveString, addCurrentToString, addAsciiToString, addControlToString :: Action
getInteger, getVariable :: Action

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

getInteger (p, _, _, input) len = if (length r == 1)
                                  then return (L p (LInt (fst (head r))) (Just s))
                                  else lexerError "Invalid number"
  where
    s = take len input
    r = readDec s

getBoolean :: Action
getBoolean (p, _, _, input) len = if s == "true"
                                  then return (L p (LBool True) (Just s))
                                  else if s == "false"
                                  then return (L p (LBool False) (Just s))
                                  else lexerError "Not a boolean."
  where s = take len input

-- | A sequence of letters is an identifier, except for reserved words, which are tested for beforehand
getVariable (p, _, _, input) len = return (L p (LIdentifier s) (Just s))
  where
    s = take len input

enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan

addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"
leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (L p (LString (reverse s)) (Just (take len input)))

-- User state

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringState   :: Bool
                     , lexerStringValue   :: String
                     -- used by the parser phase
                     , parserCollIdent    :: Map String Int
                     , parserCurrentToken :: Lexeme
                     , parserPos          :: Pos
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                     , lexerStringState   = False
                     , lexerStringValue   = ""
                     , parserCollIdent    = Map.empty
                     , parserCurrentToken = L (AlexPn 0 0 0) LEOF Nothing
                     , parserPos          = Nothing
                   }

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

-- | Get the depth of the current comment.
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

-- Error handling

-- | Throw an error according to a given message.
lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Utilities

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

type Pos = Maybe AlexPosn

-- | An EOF definition needed by Alex
alexEOF :: Alex Lexeme
alexEOF = return (L (AlexPn 0 0 0) LEOF Nothing)

-- Execution

scanner :: String -> Either String [Lexeme]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(L _ cl _) = t
                            if (cl == LEOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in runAlex str loop

-- We capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))
}
