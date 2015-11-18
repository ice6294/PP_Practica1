module Utils2 where

	import Data.List
	import Documento


	-- Ejer 1
	option1_2 :: [Document] -> IO ()
	option1_2 docs = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ orderByTitle $ filterByYear (read year::Int) docs



	-- Ejer 2
	option2_2 :: [Document] -> IO ()
	option2_2 docs = do
		putStrLn bar
		putStrLn $ showPointed $ nub $ getJournals docs -- nub -> Remove duplicates



	-- Ejer 3
	option3_2 :: [Document] -> IO ()
	option3_2 docs = do
		putStr "Acronim: "
		acronim <- getLine
		putStrLn $ "\nSearching in articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ filterByAcronim acronim docs -- nub -> Remove duplicates



	-- Ejer 4
	option4_2 :: [Document] -> IO ()
	option4_2 docs = do
		putStr "Journal: "
		journal <- getLine
		putStr "Acronym: "
		acronim <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitles $ filterByAcronim acronim $ filterByJournal journal (removeDuplicatedAcronims docs)



	-- Ejer 5
	option5_2 :: [Document] -> IO ()
	option5_2 docs = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitlesAndAcronims $ filterByYear (read year::Int) (removeDuplicatedAcronims docs)



	-- Ejer 6
	option6_2 :: [Document] -> IO ()
	option6_2 docs = do
		putStr "ID: "
		id <- getLine
		putStrLn $ "\nSearching articles...\n" ++ bar
		putStrLn $ showAllDocumentsTitlesAndAcronims2 $ filterById (read id::Int) docs




	-- Ejer 7
	option7_2 :: [Document] -> IO ()
	option7_2 docs = do
		putStrLn $ showAllDocumentsTitlesAndIds $ filterByNoAcronims docs



	-- Ejer 8
	option8_2 :: [Document] -> IO ()
	option8_2 docs = do
		putStrLn $ showAllDocuments docs



	-- Ejer 9
	option9_2 :: [Document] -> IO ()
	option9_2 docs = putStrLn "# Not implemented yet"



	-- Option All
	optionAll_2 :: [Document] -> IO ()
	optionAll_2 docs = do
		putStrLn $ showAllDocumentsAndAcronims $ orderByTitle $ removeDuplicatedAcronims docs