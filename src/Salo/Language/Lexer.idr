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
  | Bar

  | Space
  | Comment

  | DocComment String

  | Keyword String
  | Ident String
  | StringLit String

public export
Show Token where
  show Arrow          = "->"
  show Colon          = ":"
  show Equals         = "="
  show Bar            = "|"

  show Space          = "(space)"
  show Comment        = "(comment)"

  show (DocComment s) = "doc comment: " ++ show s

  show (Keyword s)    = "keyword "      ++ show s
  show (Ident s)      = "identifier "   ++ show s
  show (StringLit s)  = show s

public export
Eq Token where
  (==) x y = case (x, y) of
    (Arrow, Arrow)     => True
    (Colon, Colon)     => True
    (Equals, Equals)   => True
    (Bar, Bar)         => True

    (Space,   Space)   => True
    (Comment, Comment) => True

    (DocComment s, DocComment s') => s == s'

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

            mkDocComment : String -> Token
            mkDocComment s = DocComment s

            tokens : TokenMap Token
            tokens =
              [ (exact "->",             const Arrow)
              , (is ':',                 const Colon)
              , (is '=',                 const Equals)
              , (is '|',                 const Bar)

              , (space,                                    const Space)
              , (lineComment (exact "--"),                 const Comment)
              , (blockComment (exact "{--") (exact "--}"), const Comment)

              , (lineComment (exact "||"),                 mkDocComment)
              , (blockComment (exact "{||") (exact "||}"), mkDocComment)

              , (ident,                     kwdOrIdent)
              , (stringLit,                 parseString)
            ]

-- TODO: There are no assertions in these tests :)
namespace Test
  namespace Examples
    export
    exampleProg : String
    exampleProg = #"""
      -- This is a comment.

      || This is a doc comment
      myvalue : String
      myvalue = "This is an immutable value, myvalue, with the type of String."

      myfunction : String -> String
      myfunction "" = "This is a function that pattern matches on its first argument."
      myfunction _  = "If the first argument is a blank String, it evaluates to ^"
      -- Else, `myfunction` evaluates to ^.

      -- This is a type declaration. Here, we declare that the type `MyType` as EITHER
      -- `True` or `False`.
      type MyType = True | False

      -- This is a dependent type declaration. Here, we declare `MyDependentType` to
      -- be a type that depends on two Nats.
      type MyDependentType = Nat -> Nat

      {--
        Here's a block comment.

        We can have quite long block comments!
      --}

      {||
        Here's a block doc comment.

        We can have quite long block doc comments, too!
      ||}

      -- Functions can also be polymorphic
      f : a -> a
      f x = x

      -- `f` can now be called on anything.

      -- In this sample file, we didn't cover imports.
    """#

    export
    docComments : String
    docComments = #"""
      {||
        AAA!
      ||}
    """#

  LexerType : Type
  LexerType = Either ParseError (List (TokenData Token))

  lexExampleProg : LexerType
  lexExampleProg = Salo.Language.Lexer.lex exampleProg

  lexDocComments : LexerType
  lexDocComments = Salo.Language.Lexer.lex docComments

  testHarness : LexerType -> IO ()
  testHarness t = case t of
            Left err   => putStrLn $ "  -> Error: " ++ (show err)
            Right toks => putStrLn $ "  -> Success"

  export
  test : IO ()
  test = do putStrLn "Lexer:"
            testHarness lexExampleProg
            testHarness lexDocComments
