module Utils where

	import System.Console.ANSI
	import Data.List
	import Documento
	import Papers


	-- Ejer 1
	option1 :: IO ()
	option1 = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 1 papers [year]
		putStrLn $ showAllDocumentsTitles $ orderByTitle result



	-- Ejer 2
	option2 :: IO ()
	option2 = do
		putStrLn bar
		papers <- getPapers
		result <- getDocuments 2 papers []
		putStrLn $ showPointed $ nub $ getJournals result -- nub -> Remove duplicates



	-- Ejer 3
	option3 :: IO ()
	option3 = do
		putStr "Acronim: "
		acronim <- getLine
		putStrLn $ "\nSearching in articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 3 papers [acronim]
		putStrLn $ showAllDocumentsTitles result



	-- Ejer 4
	option4 :: IO ()
	option4 = do
		putStr "Journal: "
		journal <- getLine
		putStr "Acronym: "
		acronim <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 4 papers [journal,acronim]
		putStrLn $ showAllDocumentsTitles $ filterByAcronim acronim $ filterByJournal journal result



	-- Ejer 5
	option5 :: IO ()
	option5 = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 5 papers [year]
		putStrLn $ showAllDocumentsTitlesAndAcronims $ filterByYear (read year::Int) result



	-- Ejer 6
	option6 :: IO ()
	option6 = do
		putStr "ID: "
		id <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 6 papers [id]
		putStrLn $ showAllDocumentsTitlesAndAcronims2 $ filterById (read id::Int) result




	-- Ejer 7
	option7 :: IO ()
	option7 = do
		papers <- getPapers
		result <- getDocuments 7 papers []
		putStrLn $ showAllDocumentsTitlesAndIds $ filterByNoAcronims result



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
		putStrLn $ showAllDocumentsAndAcronims $ orderByTitle $ removeDuplicatedAcronims result
