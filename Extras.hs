module Extras where

	import System.Console.ANSI
	import System.IO
	import Data.Char

	bar = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

	prompt :: String -> IO String
	prompt text = do
		putStr text
		hFlush stdout
		getLine

	clearUp :: IO ()
	clearUp = do
		clearScreen
		cursorUp 100

	printPoints :: Int -> String
	printPoints 0 = ""
	printPoints n = "." ++ printPoints (n-1)