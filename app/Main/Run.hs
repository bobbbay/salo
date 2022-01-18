module Main.Run where

import Salo.Language.Lexer ( scanner )
import Salo.Language.Parser ( parse )

import System.Directory ( doesFileExist )
import Control.Monad.Trans
import Control.Monad ( when )
import System.IO ( hPutStrLn, stderr )

run :: String -> IO ()
run filename = do flag <- doesFileExist filename
                  when (not flag) (error ("The following file does not exist: " ++ filename))
                  s <- readFile filename
                  let tokens = Salo.Language.Lexer.scanner s
                  case tokens of
                    Left  st -> hPutStrLn stderr $ "E" ++ st
                    Right ls
                      -> do putStrLn $ show ls
                            let ast = parse ls
                            case ast of
                              Left e ->  putStrLn $ show e
                              Right a -> do putStrLn $ show a
                                            let sr = scanner s
                                            case sr of
                                              Left st  -> error st
                                              Right ls -> putStrLn (show ls)
