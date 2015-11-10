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
		result <- getDocuments papers
		putStrLn $ showAllDocumentsTitles $ orderByTitle $ filterByYear (read year::Int) result
		--putStrLn $ showAllDocuments $ orderByTitle $ filterByYear (read year::Int) result



	-- Ejer 2
	option2 :: IO ()
	option2 = do
		putStrLn bar
		papers <- getPapers
		result <- getDocuments papers
		putStrLn $ showAllDocumentsJournal $ nub result -- nub -> Remove duplicates



	-- Ejer 3
	option3 :: IO ()
	option3 = putStrLn "# Not implemented yet"



	-- Ejer 4
	option4 :: IO ()
	option4 = putStrLn "# Not implemented yet"



	-- Ejer 5
	option5 :: IO ()
	option5 = putStrLn "# Not implemented yet"



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