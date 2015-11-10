module Main(main) where

	import Documento
	import Papers

	-- Main menu
	main :: IO ()
	main = do
		putStr menu
		sel <- getLine
		select sel

	menu :: String
	menu =	"Welcome. What do you want to do?\n" ++
			"\t1) Show articles (alphabetically ordered) published in one year\n" ++
			"\t2) ...\n" ++
			"\texit) Exit\n$: "

	select :: String -> IO ()
	select sel
		| sel == "1"	= do 
			option1
			main
		| sel == "exit"	= putStrLn "Bye! :)\n\n"
		| otherwise		= do 
			putStrLn "\nSorry, not recognized buffer.\n"
			main

	-- Options (exercices)
	option1 :: IO ()
	option1 = do
		putStr "Year: "
		year <- getLine
		putStrLn "Searching articles..."
		papeles <- getPapers
		putStrLn (showAllDocuments (getDocuments papeles))