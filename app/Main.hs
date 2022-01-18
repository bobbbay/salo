module Main where

import Options.Applicative
import Data.Monoid
import Data.List

import Main.Repl ( repl )
import Main.Run ( run )

main :: IO ()
main = execParser opts >>= start

data Sample
  = Repl
  | Run String
  deriving (Eq, Show)

sample :: Parser Sample
sample = subparser
       ( command "repl"
         (info (pure Repl)
               (progDesc "Open the Salo REPL"))
       <> ( command "eval"
         (info runParser
               (progDesc "Evaluate a specific Salo file"))))

runParser :: Parser Sample
runParser = Run
       <$> argument str (metavar "FILE")

start :: Sample -> IO ()
start Repl = repl
start (Run s) = run s

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> header "salo - a toolset to agnostically build and deploy OS images remotely" )
