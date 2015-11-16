module Main(main) where

	import Documento
	import Papers
	import Utils


	-- MAIN MENU
	main :: IO ()
	main = do
		putStr menu
		sel <- getLine
		putStr bar
		select sel

	menu :: String
	menu =	bar ++ "\n· Welcome to \"Practica 1 - Haskell \". What do you want to do?\n" ++
			"\t1) Show titles of the articles (alphabetically ordered) published in one year.\n" ++
			"\t2) Show magazines where all the articles in the collection were published.\n" ++
			"\t3) Search acronym in the articles and show the titles of the ones that have it.\n" ++
			"\t4) Show titles of the articles published in a given journal that contains a given acronym.\n" ++
			"\t5) Show acronyms from the articles published in a given year.\n" ++
			"\t6) ...\n" ++
			"\t7) ...\n" ++
			"\t8) ...\n" ++
			"\t9) ...\n" ++
			"\t(all)\n"  ++
			"\t(exit)\n\n  $: "

	select :: String -> IO ()
	select sel
		| sel == "1"	= do 
			option1
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "2"	= do 
			option2
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "3"	= do 
			option3
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "4"	= do 
			option4
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "5"	= do 
			option5
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "6"	= do 
			option6
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "7"	= do 
			option7
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "8"	= do 
			option8
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "9"	= do 
			option9
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "all"	= do 
			optionAll
			putStr "Press enter:"
			x <- getLine
			main

		| sel == "exit"	= putStrLn "Bye!\n\n"

		| otherwise		= do 
			putStrLn "\nSorry, not recognized buffer.\n"
			main