module Documento where

	import System.IO
	import System.IO.Unsafe
	import Data.List
	import Control.Monad
	import Papers
	import Acronimo

	-- SCALARS
	bar = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

	-- DATAS
{-
	data Acronim = Acronim
		{	short 		:: String
		,	expanded	:: String }
-}

	data Document = Document
		{	path			:: String
		,	journal			:: String
		,	ident			:: Int
		,	year			:: Int
		,	title			:: String
		,	abstract		:: String
		,	sections		:: [[String]]
		,	acronims		:: [Acronim]	} deriving (Show)	-- QUITAR ACRONIMOS. SOLO BUSCAR SI ES NECESARIO

	instance Eq Document where
		a == b = (getTitle a) == (getTitle b)

	instance Ord Document where
		compare a b = compare (getTitle a) (getTitle b)



	-- READ ONE DOCUMENT FROM PATH
	readDocument :: String -> IO Document
	readDocument path = do
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

		acronims <- searchAcronims (resto)	-- QUICKSEARCH

		return	(Document path journal (read ident::Int) (read year::Int) title	-- Dir, Rev, ID, Año, Título
				(take 180 abstract ++ " ...") (getRest $ init $ lines resto) (removeDuplicated acronims))	-- Resumen, Secciones, Acronimos

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
	readDocument_1 :: String -> IO Document
	readDocument_1 path = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		year <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título

		return (Document path "" 0 (read year::Int) title "" [] [])

	readDocument_2 :: String -> IO Document
	readDocument_2 path = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle			-- Revista

		return (Document path journal 0 0 "" "" [] [])

	readDocument_3 :: String -> IO Document
	readDocument_3 path = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronims <- quickSearch resto

		return	(Document path "" 0 0 title "" [] (removeDuplicated acronims))

	readDocument_4 :: String -> IO Document
	readDocument_4 path = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle		-- Revista
		no <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronims <- searchAcronims resto

		return	(Document path journal 0 0 title "" [] (removeDuplicated acronims))

	readDocument_5 :: String -> IO Document
	readDocument_5 path = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		no <- hGetLine handle			-- ID
		year <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronims <- searchAcronims resto

		return	(Document path "" 0 (read year::Int) title "" [] (removeDuplicated acronims))

	readDocument_6 :: String -> IO Document
	readDocument_6 path = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		id <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronims <- searchAcronims resto

		return	(Document path "" (read id::Int) 0 title "" [] acronims)

	readDocument_7 :: String -> IO Document
	readDocument_7 path = do
		handle <- openFile path ReadMode
		no <- hGetLine handle			-- Revista
		id <- hGetLine handle			-- ID
		no <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- Resumen
		no <- hGetLine handle			-- (--)

		resto <- hGetContents handle	-- resto

		acronims <- searchAcronims resto

		return	(Document path "" (read id::Int) 0 title "" [] (removeDuplicated acronims))

	readDocument_8 :: String -> IO Document
	readDocument_8 path = do
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

		return	(Document path journal (read ident::Int) (read year::Int) title	-- Dir, Rev, ID, Año, Título
				(take 180 abstract ++ " ...") (getRest $ init $ lines resto) [])	-- Resumen, Secciones, Acronimos





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

	showAllDocumentsTitlesAndAcronims :: [Document] -> String
	showAllDocumentsTitlesAndAcronims (d:ds) = "· " ++ getTitle d ++ "\n" ++
		showAcronims (getAcronims d) ++ showAllDocumentsTitlesAndAcronims ds
	showAllDocumentsTitlesAndAcronims [] = ""

	showAllDocumentsTitlesAndAcronims2 :: [Document] -> String
	showAllDocumentsTitlesAndAcronims2 (d:ds) = "· " ++ getTitle d ++ "\n" ++
		showPointed (countDuplicates $ getAcronims d) ++ showAllDocumentsTitlesAndAcronims2 ds
	showAllDocumentsTitlesAndAcronims2 [] = ""

	showAllDocumentsTitlesAndIds :: [Document] -> String
	showAllDocumentsTitlesAndIds (d:ds) = "· " ++ getTitle d ++ ". Id: " ++ (show $ getIdent d) ++"\n" ++ showAllDocumentsTitlesAndIds ds
	showAllDocumentsTitlesAndIds [] = ""

	showAllDocumentsAndAcronims :: [Document] -> String
	showAllDocumentsAndAcronims (d:ds) = showDocumentAndAcronims d ++ "\n" ++ showAllDocumentsAndAcronims ds
	showAllDocumentsAndAcronims [] = ""

	showDocumentAndAcronims :: Document -> String
	showDocumentAndAcronims doc =	"\nPath: " ++ getPath doc ++ "\nTitle: " ++ getTitle doc ++ " (" ++ (show $ getYear doc) ++
						")\nAbstract: " ++ getAbstract doc ++ "\nSection number: " ++
						(show $ getSectionsNumber doc) ++ "\nSections:\n" ++ showTitles (getTitles doc)
						++ "Acronimos: \n" ++ showAcronims (getAcronims doc)

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

	getAcronims :: Document -> [Acronim]
	getAcronims (Document _ _ _ _ _ _ _ acronims) = acronims


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
	getDocuments :: Int -> [String] -> IO [Document]
	getDocuments n (l:ls)
		| n == 1	=	do
			x <- readDocument_1 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 2	=	do
			x <- readDocument_2 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 3	=	do
			x <- readDocument_3 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 4	=	do
			x <- readDocument_4 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 5	=	do
			x <- readDocument_5 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 6	=	do
			x <- readDocument_6 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 7	=	do
			x <- readDocument_7 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| n == 8	=	do
			x <- readDocument_8 l
			xs <- getDocuments n ls
			return $ [x] ++ xs
		| otherwise =	do
			x <- readDocument l
			xs <- getDocuments n ls
			return $ [x] ++ xs
	getDocuments _ [] = return []


	-- PROCESS BUFFER OF DOCUMENTS
	filterByYear :: Int -> [Document] -> [Document]
	filterByYear year list = filter aux list
		where
			aux d = getYear d == year

	filterByAcronim :: String -> [Document] -> [Document]
	filterByAcronim acronim list = filter aux list
		where
			aux d = 0 /= (length $ filter ((Acronim acronim "" 0) ==) (getAcronims d))

	filterByJournal :: String -> [Document] -> [Document]
	filterByJournal journal list = filter aux list
		where
			aux d = getJournal d == journal

	filterById :: Int -> [Document] -> [Document]
	filterById id list = filter aux list
		where
			aux d = getIdent d == id

	filterByNoAcronims :: [Document] -> [Document]
	filterByNoAcronims list = filter aux list
		where
			aux d = (length $ getAcronims d) == 0

	orderByTitle :: [Document] -> [Document]
	orderByTitle list = sort list

	getJournals :: [Document] -> [String]
	getJournals [] = []
	getJournals (d:ds) = [getJournal d] ++ getJournals ds