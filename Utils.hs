module Utils where

	import System.Console.ANSI
	import Data.Char
	import Data.List
	import Documento
	import Papers
	import Extras
	import Aux

	-- SCALARS
	options = [optionAll,option1,option2,option3,option4,option5,option6,option7,option8,option9]



	-- Ejer 1
	option1 :: IO ()
	option1 = do
		year <- prompt "Year: "
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 1 papers [year]
		putStrLn $ showAllDocumentsTitles $ orderByTitle result



	-- Ejer 2
	option2 :: IO ()
	option2 = do
		--putStrLn bar
		papers <- getPapers
		result <- getDocuments 2 papers []
		putStrLn $ showPointed $ nub $ getJournals result -- nub -> Remove duplicates



	-- Ejer 3
	option3 :: IO ()
	option3 = do
		acronim <- prompt "Acronym: "
		putStrLn $ "\nSearching in articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 3 papers [acronim]
		putStrLn $ showAllDocumentsTitles result



	-- Ejer 4
	option4 :: IO ()
	option4 = do
		journal <- prompt "Journal: "
		acronym <- prompt "Acronym: "
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 4 papers [journal,acronym]
		putStrLn $ showAllDocumentsTitles $ filterByAcronym acronym $ filterByJournal journal result



	-- Ejer 5
	option5 :: IO ()
	option5 = do
		year <- prompt "Year: "
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 5 papers [year]
		putStrLn $ showAllDocumentsTitlesAndAcronyms $ filterByYear (read year::Int) result



	-- Ejer 6
	option6 :: IO ()
	option6 = do
		id <- prompt "ID: "
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 6 papers [id]
		putStrLn $ showAllDocumentsTitlesAndAcronyms2 $ filterById (read id::Int) result



	-- Ejer 7
	option7 :: IO ()
	option7 = do
		papers <- getPapers
		result <- getDocuments 7 papers []
		putStrLn $ showAllDocumentsTitlesAndIds $ filterByNoAcronyms result



	-- Ejer 8
	option8 :: IO ()
	option8 = do
		papers <- getPapers
		result <- getDocuments 8 papers []
		putStrLn $ showAllDocuments result



	-- Ejer 9
	option9 :: IO ()
	option9 = putStrLn "# Not implemented yet"



	-- Option All
	optionAll :: IO ()
	optionAll = do
		papers <- getPapers
		result <- getDocuments 0 papers []
		clearLine
		--putStrLn $ showAllDocumentsAndAcronyms $ orderByTitle $ removeDuplicatedAcronyms result
		showOneByOne (orderByTitle $ removeDuplicatedAcronyms result) 0

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