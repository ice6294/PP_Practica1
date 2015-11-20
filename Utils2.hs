module Utils2 where

	import System.Console.ANSI
	import Data.Char
	import Data.List
	import Documento
	import Extras
	import Aux

	-- SCALARS
	options_2 = [optionAll_2,option1_2,option2_2,option3_2,option4_2,option5_2,option6_2,option7_2,option8_2,option9_2]



	-- Ejer 1
	option1_2 :: [Document] -> IO ()
	option1_2 docs = do
		year <- prompt "Year: "
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ orderByTitle $ filterByYear (read year::Int) docs



	-- Ejer 2
	option2_2 :: [Document] -> IO ()
	option2_2 docs = do
		--putStrLn bar
		putStrLn $ showPointed $ nub $ getJournals docs -- nub -> Remove duplicates



	-- Ejer 3
	option3_2 :: [Document] -> IO ()
	option3_2 docs = do
		acronym <- prompt "Acronym: "
		putStrLn $ "\nSearching in articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ filterByAcronym acronym docs -- nub -> Remove duplicates



	-- Ejer 4
	option4_2 :: [Document] -> IO ()
	option4_2 docs = do
		journal <- prompt "Journal: "
		acronym <- prompt "Acronym: "
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ filterByAcronym acronym $ filterByJournal journal (removeDuplicatedAcronyms docs)



	-- Ejer 5
	option5_2 :: [Document] -> IO ()
	option5_2 docs = do
		year <- prompt "Year: "
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitlesAndAcronyms $ filterByYear (read year::Int) (removeDuplicatedAcronyms docs)



	-- Ejer 6
	option6_2 :: [Document] -> IO ()
	option6_2 docs = do
		id <- prompt "ID: "
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitlesAndAcronyms2 $ filterById (read id::Int) docs




	-- Ejer 7
	option7_2 :: [Document] -> IO ()
	option7_2 docs = do
		putStrLn $ showAllDocumentsTitlesAndIds $ filterByNoAcronyms docs



	-- Ejer 8
	option8_2 :: [Document] -> IO ()
	option8_2 docs = do
		putStrLn $ showAllDocuments docs



	-- Ejer 9
	option9_2 :: [Document] -> IO ()
	option9_2 docs = do
		clustering $ sortByAuxLength $ readExtras docs



	-- Option All
	optionAll_2 :: [Document] -> IO ()
	optionAll_2 docs = do
		showOneByOne (orderByTitle $ removeDuplicatedAcronyms docs) 0

	showOneByOne :: [Document] -> Int -> IO ()
	showOneByOne docs (-1) = showOneByOne docs 0
	showOneByOne docs i = do
		if (i < length docs) then
			do
				clearUp
				putStrLn ((show i) ++ ") " ++ bar ++ showDocumentAndAcronyms (docs !! i) ++ "\n")
				sel <- prompt "prev(p) / next(n) / (stop) / nº  $ "
				if (sel == "p") then
					showOneByOne docs (i-1)
				else if (sel == "n" || sel == "") then
					showOneByOne docs (i+1)
				else if (sel == "stop") then
					putStrLn ""
				else if (all isDigit sel) then
					showOneByOne docs (read sel::Int)
				else
					showOneByOne docs (i)
		else
			putStrLn ""



	-- Option Top
	optionTop_2 :: [Document] -> IO ()
	optionTop_2 docs = do
		clearUp
		putStrLn $ showExtras $ sortByAuxLength $ readExtras docs