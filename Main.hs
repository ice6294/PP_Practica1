module Main(main) where

	import System.Console.ANSI
	import Documento
	import Papers
	import Utils
	import Utils2

	-- MAIN MENU
	main :: IO ()
	main = do
		clearScreen
		cursorUp 100
		putStr mode_menu
		sel <- getLine
		if (sel == "1") then
			start
		else if (sel == "2") then
			do
				clearScreen
				cursorUp 100
				papers <- getPapers
				docs <- getAllDocuments 0 (length papers) papers
				start2 docs
		else if (sel == "exit") then
			putStrLn "Bye!\n\n"
		else
			do
				putStrLn "\nSorry, not recognized buffer.\n"
				putStr "Press enter:"
				x <- getLine
				main

	mode_menu :: String
	mode_menu = bar ++ "\n· Welcome to \"PP - Practica 1 - Haskell \". Which mode do you prefer?\n" ++
				"\t1) Operate with the documents while use them.\n\t2) Charge all documents before use them." ++
				"\n\t(exit)\n\n   $: "

	menu :: String
	menu =	bar ++ "\nWhat do you want to do?\n" ++
			"\t1) Show titles of the articles (alphabetically ordered) published in one year.\n" ++
			"\t2) Show magazines where all the articles in the collection were published.\n" ++
			"\t3) Search acronym in the articles and show the titles of the ones that have it.\n" ++
			"\t4) Show titles of the articles published in a given journal that contains a given acronym.\n" ++
			"\t5) Show acronyms from the articles published in a given year.\n" ++
			"\t6) Show acronyms with the numer of times that appears in an article from a given ID.\n" ++
			"\t7) Show all IDs and titles of the articles without any acronyms.\n" ++
			"\t8) Show all information from all articles.\n" ++
			"\t9) ...\n" ++
			"\t(all)\n"  ++
			"\t(mode)\n" ++
			"\t(exit)\n\n   $: "

	start :: IO ()
	start = do
		clearScreen
		cursorUp 100
		putStr menu
		sel <- getLine
		putStr bar
		select sel

	select :: String -> IO ()
	select sel
		| sel == "1"	= do 
			option1
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "2"	= do 
			option2
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "3"	= do 
			option3
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "4"	= do 
			option4
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "5"	= do 
			option5
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "6"	= do 
			option6
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "7"	= do 
			option7
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "8"	= do 
			option8
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "9"	= do 
			option9
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "all"	= do 
			optionAll
			putStr "Press enter:"
			x <- getLine
			start

		| sel == "mode"	= do 
			main

		| sel == "exit"	= putStrLn "Bye!\n\n"

		| otherwise		= do 
			putStrLn "\nSorry, not recognized buffer.\n"
			putStr "Press enter:"
			x <- getLine
			start


	start2 :: [Document] -> IO ()
	start2 documents = do
		clearScreen
		cursorUp 100
		putStr menu
		sel <- getLine
		putStr bar
		select2 sel documents

	select2 :: String -> [Document] -> IO ()
	select2 sel docs
		| sel == "1"	= do 
			option1_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "2"	= do 
			option2_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "3"	= do 
			option3_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "4"	= do 
			option4_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "5"	= do 
			option5_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "6"	= do 
			option6_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "7"	= do 
			option7_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "8"	= do 
			option8_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "9"	= do 
			option9_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "all"	= do 
			optionAll_2 docs
			putStr "Press enter:"
			x <- getLine
			start2 docs

		| sel == "mode"	= do 
			main

		| sel == "exit"	= putStrLn "Bye!\n\n"

		| otherwise		= do 
			putStrLn "\nSorry, not recognized buffer.\n"
			start2 docs