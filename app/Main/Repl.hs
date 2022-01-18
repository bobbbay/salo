module Main.Repl where

import Salo.Language.Syntax ( Expr )
import Salo.Language.Parser ( parseExpr, parseTokens )

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input = do
  let tokens = parseTokens $ replPreprocess input
  putStrLn (show tokens)
  let ast = parseExpr input
  case ast of
    Left err -> do
      putStrLn $ "Parse Error: " ++ err
    Right ast -> putStrLn "Success!"

-- | Preprocess a REPL line to be valid Salo syntax.
replPreprocess :: String -> String
replPreprocess x = "module Repl\n" ++ x

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Salo λ> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
