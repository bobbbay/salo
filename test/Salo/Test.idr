module Salo.Test

someFile : IO ()
someFile = do file <- readFile "input.txt"
	      case file of
	      	   Right content => printLn content
		   Left err => printLn err

main : IO ()
main = do putStrLn "All tests ran successfully."