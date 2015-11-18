module Documento where

	import System.Console.ANSI
	import System.IO
	import System.IO.Unsafe
	import Control.Monad
	import Data.List
	import Papers
	import Acronimo

	-- SCALARS
	bar = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
	read_funcions = [readDocument,readDocument_1,readDocument_2,readDocument_3,readDocument_4,readDocument_5,readDocument_6,readDocument_7,readDocument_8]

	-- DATAS
	data Document = Document
		{	path			:: String
		,	journal			:: String
		,	ident			:: Int
		,	year			:: Int
		,	title			:: String
		,	abstract		:: String
		,	sections		:: [[String]]
		,	acronyms		:: [Acronym]	} deriving (Show)

	instance Eq Document where
		a == b = (getTitle a) == (getTitle b)

	instance Ord Document where
		compare a b = compare (getTitle a) (getTitle b)



	-- READ ONE DOCUMENT FROM PATH
	readDocument :: String -> [String] -> IO [Document]
	readDocument path _ = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle		-- Revista
		ident <- hGetLine handle		-- ID
		year <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- (--)
		abstract <- hGetLine handle		-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronyms <- searchAcronyms (resto)	-- QUICKSEARCH

		return	[(Document path journal (read ident::Int) (read year::Int) title	-- Dir, Rev, ID, Año, Título
				(take 180 abstract ++ " ...") (getRest $ init $ lines resto) acronyms)]	-- Resumen, Secciones, Acronimos

	getRest :: [String] -> [[String]]
	--getRest ["--"] = []
	getRest ("--":ds) = getRest ds
	getRest (d:ds) = 
		if head ds == "--" then
			[[d,""]] ++ getRest (tail ds)
		else
			[[d,head ds]] ++ getRest (tail ds)
	getRest [] = []
	{- NOTA!: SI UNA SECCION TIENE VARIOS SALOS DE LINEA, NO FUNCIONA BIEN! -}



	-- READ BY OPTION
	readDocument_1 :: String -> [String] -> IO [Document]
	readDocument_1 path args = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		year <- hGetLine handle			-- Año
		if (year == (args !! 0)) then
			do
				no <- hGetLine handle 			-- (--)
				title <- hGetLine handle		-- Título
				return [(Document path "" 0 (read year::Int) title "" [] [])]
		else
			return []

	readDocument_2 :: String -> [String] -> IO [Document]
	readDocument_2 path _ = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle			-- Revista

		return [(Document path journal 0 0 "" "" [] [])]

	readDocument_3 :: String -> [String] -> IO [Document]
	readDocument_3 path args = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronyms <- quickSearch resto

		if (containsAcronym acronyms (args !! 0)) then
			return [(Document path "" 0 0 title "" [] (removeDuplicates acronyms))]
		else
			return []

	readDocument_4 :: String -> [String] -> IO [Document]
	readDocument_4 path args = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle		-- Revista
		if (journal == (args !! 0)) then
			do
				no <- hGetLine handle			-- ID
				no <- hGetLine handle			-- Año
				no <- hGetLine handle 			-- (--)
				title <- hGetLine handle		-- Título
				no <- hGetLine handle			-- Resumen
				no <- hGetLine handle			-- (--)

				resto <- hGetContents handle	-- resto

				acronyms <- searchAcronyms resto

				if (containsAcronym acronyms (args !! 1)) then
					return	[(Document path journal 0 0 title "" [] (removeDuplicates acronyms))]
				else
					return []
		else
			return []

	readDocument_5 :: String -> [String] -> IO [Document]
	readDocument_5 path args = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		year <- hGetLine handle			-- Año
		if (year == (args !! 0)) then
			do
				no <- hGetLine handle 			-- (--)
				title <- hGetLine handle		-- Título
				no <- hGetLine handle			-- Resumen
				no <- hGetLine handle			-- (--)

				resto <- hGetContents handle	-- resto

				acronyms <- searchAcronyms resto

				return	[(Document path "" 0 (read year::Int) title "" [] (removeDuplicates acronyms))]
		else
			return []

	readDocument_6 :: String -> [String] -> IO [Document]
	readDocument_6 path args = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		id <- hGetLine handle			-- ID
		if (id == (args !! 0)) then
			do
				no <- hGetLine handle			-- Año
				no <- hGetLine handle 			-- (--)
				title <- hGetLine handle		-- Título
				no <- hGetLine handle			-- Resumen
				no <- hGetLine handle			-- (--)

				resto <- hGetContents handle	-- resto

				acronyms <- searchAcronyms resto

				return	[(Document path "" (read id::Int) 0 title "" [] acronyms)]
		else
			return []

	readDocument_7 :: String -> [String] -> IO [Document]
	readDocument_7 path _ = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		id <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronyms <- searchAcronyms resto
		if (acronyms == []) then
			return	[(Document path "" (read id::Int) 0 title "" [] (removeDuplicates acronyms))]
		else
			return []

	readDocument_8 :: String -> [String] -> IO [Document]
	readDocument_8 path _ = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle		-- Revista
		ident <- hGetLine handle		-- ID
		year <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- (--)
		abstract <- hGetLine handle		-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		return	[(Document path journal (read ident::Int) (read year::Int) title	-- Dir, Rev, ID, Año, Título
				(take 180 abstract ++ " ...") (getRest $ init $ lines resto) [])]	-- Resumen, Secciones, Acronimos



	-- SHOW FUNCTIONS
	showAllDocuments :: [Document] -> String
	showAllDocuments (d:ds) = showDocument d ++ "\n" ++ showAllDocuments ds
	showAllDocuments [] = ""

	showDocument :: Document -> String
	showDocument doc =	"\nTitle: " ++ getTitle doc ++ " (" ++ (show $ getYear doc) ++
						")\nAbstract: " ++ getAbstract doc ++ "\nSection number: " ++
						(show $ getSectionsNumber doc) ++ "\nSections:\n" ++ showTitles (getTitles doc)

	showTitles :: [String] -> String
	showTitles [] = ""
	showTitles (t:ts) = "\t· " ++ t ++ "\n" ++ showTitles ts

	showAllDocumentsTitles :: [Document] -> String
	showAllDocumentsTitles (d:ds) = "\t· " ++ getTitle d ++ "\n" ++ showAllDocumentsTitles ds
	showAllDocumentsTitles [] = ""

	showAllDocumentsJournal :: [Document] -> String
	showAllDocumentsJournal (d:ds) = "\t· " ++ getJournal d ++ "\n" ++ showAllDocumentsJournal ds
	showAllDocumentsJournal [] = ""

	showPointed :: [String] -> String
	showPointed [] = ""
	showPointed (x:xs) = "\t· " ++ x ++ "\n" ++ showPointed xs

	showAllDocumentsTitlesAndAcronyms :: [Document] -> String
	showAllDocumentsTitlesAndAcronyms (d:ds) = "· " ++ getTitle d ++ "\n" ++
		showAcronyms (getAcronyms d) ++ showAllDocumentsTitlesAndAcronyms ds
	showAllDocumentsTitlesAndAcronyms [] = ""

	showAllDocumentsTitlesAndAcronyms2 :: [Document] -> String
	showAllDocumentsTitlesAndAcronyms2 (d:ds) = "· " ++ getTitle d ++ "\n" ++
		showPointed (countDuplicates $ getAcronyms d) ++ showAllDocumentsTitlesAndAcronyms2 ds
	showAllDocumentsTitlesAndAcronyms2 [] = ""

	showAllDocumentsTitlesAndIds :: [Document] -> String
	showAllDocumentsTitlesAndIds (d:ds) = "· " ++ getTitle d ++ ". Id: " ++ (show $ getIdent d) ++"\n" ++ showAllDocumentsTitlesAndIds ds
	showAllDocumentsTitlesAndIds [] = ""

	showAllDocumentsAndAcronyms :: [Document] -> String
	showAllDocumentsAndAcronyms (d:ds) = showDocumentAndAcronyms d ++ "\n" ++ showAllDocumentsAndAcronyms ds
	showAllDocumentsAndAcronyms [] = ""

	showDocumentAndAcronyms :: Document -> String
	showDocumentAndAcronyms doc =	"\nPath: " ++ getPath doc ++ "\nTitle: " ++ getTitle doc ++ " (" ++ (show $ getYear doc) ++
						")\nAbstract: " ++ getAbstract doc ++ "\nSection number: " ++
						(show $ getSectionsNumber doc) ++ "\nSections:\n" ++ showTitles (getTitles doc)
						++ "Acronimos: \n" ++ showAcronyms (getAcronyms doc)

						

	-- GET FUNCTIONS
	getPath :: Document -> String
	getPath (Document path _ _ _ _ _ _ _) = path

	getJournal :: Document -> String
	getJournal (Document _ journal _ _ _ _ _ _) = journal

	getIdent :: Document -> Int
	getIdent (Document _ _ ident _ _ _ _ _) = ident

	getYear :: Document -> Int
	getYear (Document _ _ _ year _ _ _ _) = year

	getTitle :: Document -> String
	getTitle (Document _ _ _ _ title _ _ _) = title

	getAbstract :: Document -> String
	getAbstract (Document _ _ _ _ _ abstract _ _) = abstract

	getSectionsNumber :: Document -> Int
	getSectionsNumber (Document _ _ _ _ _ _ sections _) = length sections

	getAcronyms :: Document -> [Acronym]
	getAcronyms (Document _ _ _ _ _ _ _ acronyms) = acronyms


	-- OTHER GET FUNCTIONS
	getTitles :: Document -> [String]
	getTitles (Document _ _ _ _ _ _ sections _) = getTitles' sections

	-- El argumento está compuesto por [[Titulo1,Seccion1], ..., [TituloN,SeccionN]] y devuelve [Titulo1, ..., TituloN]
	getTitles' :: [[String]] -> [String]
	getTitles' [[]] = []
	getTitles' (t:ts) = [head t] ++ getTitles' ts
	getTitles' _ = []



	-- RETURN ALL DOCUMENTS
	-- n: indica la manera de leer los documentos
	getDocuments :: Int -> [String] -> [String] -> IO [Document]
	getDocuments n (l:ls) args = do
		x <- (read_funcions !! n) l args -- readDdocument_# path args
		xs <- getDocuments n ls args
		return $ x ++ xs
	getDocuments _ [] _ = return []

	getAllDocuments :: Int -> Int -> [String] -> IO [Document]
	getAllDocuments n1 n2 (l:ls) = do
		x <- readDocument l []
		clearScreen
		cursorUp 100
		putStrLn $ "Cargando " ++ printPoints n1 ++ " (" ++ (show n1) ++ " de " ++ (show n2) ++ ")\n"
		xs <- getAllDocuments (n1+1) n2 ls
		return $ x ++ xs
	getAllDocuments _ _ [] = return []

	printPoints :: Int -> String
	printPoints 0 = ""
	printPoints n = "." ++ printPoints (n-1)



	-- OTHERS
	filterByYear :: Int -> [Document] -> [Document]
	filterByYear year list = filter aux list
		where
			aux d = getYear d == year

	filterByAcronym :: String -> [Document] -> [Document]
	filterByAcronym acronym list = filter aux list
		where
			aux d = 0 /= (length $ filter ((Acronym acronym "" 0) ==) (getAcronyms d))

	filterByJournal :: String -> [Document] -> [Document]
	filterByJournal journal list = filter aux list
		where
			aux d = getJournal d == journal

	filterById :: Int -> [Document] -> [Document]
	filterById id list = filter aux list
		where
			aux d = getIdent d == id

	filterByNoAcronyms :: [Document] -> [Document]
	filterByNoAcronyms list = filter aux list
		where
			aux d = (length $ getAcronyms d) == 0

	orderByTitle :: [Document] -> [Document]
	orderByTitle list = sort list

	getJournals :: [Document] -> [String]
	getJournals [] = []
	getJournals (d:ds) = [getJournal d] ++ getJournals ds

	containsAcronym :: [Acronym] -> String -> Bool
	containsAcronym [] _ = False
	containsAcronym (a:as) acronym =
		if (getAcr a == acronym) then
			True
		else
			containsAcronym as acronym

	removeDuplicatedAcronyms :: [Document] -> [Document]
	removeDuplicatedAcronyms [] = []
	removeDuplicatedAcronyms ((Document path journal ident year title abstract sections acronyms):ds) =
		[(Document path journal ident year title abstract sections (removeDuplicates acronyms))] ++ removeDuplicatedAcronyms ds