module Main.Repl where

import           Salo.Language.Lexer            ( scanner )
import           Salo.Language.Syntax           ( Salo )
import           Salo.Language.Parser           ( parse )

import           Control.Monad.Trans
import           System.Console.Haskeline
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

process :: String -> IO ()
process input = do
  putStrLn $ show input
  let tokens = Salo.Language.Lexer.scanner input
  case tokens of
    Left  st -> hPutStrLn stderr $ st
    Right ls -> do
      putStrLn $ show ls
      let ast = parse ls
      case ast of
        Left  e -> putStrLn $ show e
        Right a -> putStrLn $ show a

-- | Preprocess a REPL line to be valid Salo syntax.
replPreprocess :: String -> String
replPreprocess x = "module Repl\n" ++ x

repl :: IO ()
repl = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "Salo λ> "
    case minput of
      Nothing    -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
