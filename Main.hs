module Main(main) where

	import Documento
	import Papers
	import Utils


	-- Main menu
	main :: IO ()
	main = do
		putStr menu
		sel <- getLine
		putStr bar
		select sel

	menu :: String
	menu =	bar ++ "\n· Welcome to \"Practica 1 - Haskell \". What do you want to do?\n" ++
			"\t1) Show articles (alphabetically ordered) published in one year\n" ++
			"\t2) ...\n" ++
			"\t3) ...\n" ++
			"\t4) ...\n" ++
			"\t5) ...\n" ++
			"\t6) ...\n" ++
			"\t7) ...\n" ++
			"\t8) ...\n" ++
			"\t9) ...\n" ++
			"\t(exit)\n\n  $: "

	select :: String -> IO ()
	select sel
		| sel == "1"	= do 
			option1
			main

		| sel == "2"	= do 
			option2
			main

		| sel == "3"	= do 
			option3
			main

		| sel == "4"	= do 
			option4
			main

		| sel == "5"	= do 
			option5
			main

		| sel == "6"	= do 
			option6
			main

		| sel == "7"	= do 
			option7
			main

		| sel == "8"	= do 
			option8
			main

		| sel == "9"	= do 
			option9
			main

		| sel == "exit"	= putStrLn "Bye! :)\n\n"

		| otherwise		= do 
			putStrLn "\nSorry, not recognized buffer.\n"
			main