module Main where

import Options.Applicative
import Data.Monoid
import Data.List

import Main.Repl ( repl )

main :: IO ()
main = execParser opts >>= run

data Sample
  = Repl
  deriving (Eq, Show)

sample :: Parser Sample
sample = subparser
       ( command "repl"
         (info (pure Repl)
               (progDesc "Open the Salo REPL"))
       )

run :: Sample -> IO ()
run Repl = repl

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> header "salo - a toolset to agnostically build and deploy OS images remotely" )
