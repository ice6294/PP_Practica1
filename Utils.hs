module Utils where

	import Documento
	import Papers


	-- Ejer 1
	option1 :: IO ()
	option1 = do
		putStr "Year: "
		year <- getLine
		putStrLn $ "Searching articles...\n" ++ bar
		papers <- getPapers
		result <- getDocuments papers
		putStrLn $ showAllDocuments $ orderByTitle $ filterByYear (read year::Int) result


	-- Ejer 2
	option2 :: IO ()
	option2 = putStrLn "# Not implemented yet"


	-- Ejer 3
	option3 :: IO ()
	option3 = putStrLn "# Not implemented yet"


	-- Ejer 4
	option4 :: IO ()
			"\t4) ...\n" ++
			"\t5) ...\n" ++
			"\t6) ...\n" ++
			"\t7) ...\n" ++
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