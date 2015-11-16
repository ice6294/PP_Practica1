module Utils where

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
		result <- getDocuments 1 papers
		putStrLn $ showAllDocumentsTitles $ orderByTitle $ filterByYear (read year::Int) result



	-- Ejer 2
	option2 :: IO ()
	option2 = do
		putStrLn bar
		papers <- getPapers
		result <- getDocuments 2 papers
		putStrLn $ showPointed $ nub $ getJournals result -- nub -> Remove duplicates



	-- Ejer 3
	option3 :: IO ()
	option3 = do
		putStr "Acronim: "
		acronim <- getLine
		putStrLn $ "\nSearching in articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 3 papers
		putStrLn $ showAllDocumentsTitles $ filterByAcronim acronim result -- nub -> Remove duplicates



	-- Ejer 4
	option4 :: IO ()
	option4 = do
		putStr "Journal: "
		journal <- getLine
		putStr "Acronym: "
		acronim <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 4 papers
		putStrLn $ showAllDocumentsTitlesAndAcronims $ filterByAcronim acronim $ filterByJournal journal result



	-- Ejer 5
	option5 :: IO ()
	option5 = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar

		papers <- getPapers
		result <- getDocuments 5 papers
		putStrLn $ showAllDocumentsTitlesAndAcronims $ filterByYear (read year::Int) result



	-- Ejer 6
	option6 :: IO ()
	option6 = putStrLn "# Not implemented yet"



	-- Ejer 7
	option7 :: IO ()
	option7 = putStrLn "# Not implemented yet"



	-- Ejer 8
	option8 :: IO ()
	option8 = putStrLn "# Not implemented yet"



	-- Ejer 9
	option9 :: IO ()
	option9 = putStrLn "# Not implemented yet"



	-- Option All
	optionAll :: IO ()
	optionAll = do
		papers <- getPapers
		result <- getDocuments 0 papers
		putStrLn $ showAllDocuments $ orderByTitle result
