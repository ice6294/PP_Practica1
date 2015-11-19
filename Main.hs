module Main(main) where

	import Data.Char
	import Documento
	import Papers
	import Utils
	import Utils2
	import Extras



	-- MAIN MENU
	main :: IO ()
	main = do
		--hSetBuffering stdin NoBuffering
		clearUp
		putStr mode_menu
		sel <- prompt "   $: "
		if (sel == "1") then
			start
		else if (sel == "2") then
			do
				clearUp
				papers <- getPapers
				docs <- getAllDocuments 0 (length papers) papers
				start2 docs
		else if (sel == "exit") then
			do
				putStrLn "Bye!\n\n"
		else
			do
				putStrLn $ "\nSorry, not recognized buffer \"" ++ sel ++ "\"\n"
				putStr "Press enter:"
				x <- getLine
				main

	mode_menu :: String
	mode_menu = bar ++ "· Welcome to \"PP - Practica 1 - Haskell \". Which mode do you prefer?\n\n" ++
				"\t1) Operate with the documents while use them.\n\t2) Charge all documents before use them." ++
				"\n\t(exit)\n\n"

	menu :: String
	menu =	bar ++ "· What do you want to do?\n\n" ++
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
			"\t(exit)\n\n"

	start :: IO ()
	start = do
		clearUp
		putStr menu
		sel <- prompt "   $: "
		putStr bar
		select sel

	start2 :: [Document] -> IO ()
	start2 documents = do
		clearUp
		putStr menu
		sel <- prompt "   $: "
		putStr bar
		select2 sel documents

	select :: String -> IO ()
	select sel =
		if (length sel == 1 && isDigit(head sel)) then
			do
				(options !! digitToInt (head sel))
				x <- prompt "Press enter:"
				start
		else if (sel == "all") then
			do
				(options !! 0)
				x <- prompt "Press enter:"
				start
		else if (sel == "exit") then
			do
				putStrLn "Bye!\n\n"
		else if (sel == "mode") then
			do
				main
		else
			do
				putStrLn $ "\nSorry, not recognized buffer \"" ++ sel ++ "\"\n"
				x <- prompt "Press enter:"
				start

	select2 :: String -> [Document] -> IO ()
	select2 sel docs =
		if (length sel == 1 && isDigit(head sel)) then
			do
				(options_2 !! digitToInt (head sel)) docs
				x <- prompt "Press enter:"
				start2 docs
		else if (sel == "all") then
			do
				(options_2 !! 0) docs
				x <- prompt "Press enter:"
				start2 docs
		else if (sel == "exit") then
			do
				putStrLn "Bye!\n\n"
		else
			do
				putStrLn $ "\nSorry, not recognized buffer \"" ++ sel ++ "\"\n"
				x <- prompt "Press enter:"
				start2 docs