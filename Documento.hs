module Documento where

	import System.IO
	import System.IO.Unsafe
	import Data.List
	import Control.Monad
	import Papers

	-- SCALARS
	bar = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"



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
		--,	sections		:: [String]
		{-,	section_number	:: Int
		,	acronims		:: [Acronim]-} } deriving (Show)

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

		return	(Document path journal (read ident::Int) (read year::Int) title	-- Dir, Rev, ID, Año, Título
				(take 180 abstract ++ " ...") (getRest $ init $ lines resto))	-- Resumen, Secciones

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



	-- SHOW FUNCTIONS
	showAllDocuments :: [Document] -> String
	showAllDocuments (d:ds) = showDocument d ++ "\n" ++ showAllDocuments ds
	showAllDocuments [] = ""

	showDocument :: Document -> String
	showDocument doc =	"Title: " ++ getTitle doc ++ " (" ++ (show $ getYear doc) ++
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



	-- GET FUNCTIONS
	getPath :: Document -> String
	getPath (Document path _ _ _ _ _ _) = path

	getJournal :: Document -> String
	getJournal (Document _ journal _ _ _ _ _) = journal

	getIdent :: Document -> Int
	getIdent (Document _ _ ident _ _ _ _) = ident

	getYear :: Document -> Int
	getYear (Document _ _ _ year _ _ _) = year

	getTitle :: Document -> String
	getTitle (Document _ _ _ _ title _ _) = title

	getAbstract :: Document -> String
	getAbstract (Document _ _ _ _ _ abstract _) = abstract

	getSectionsNumber :: Document -> Int
	getSectionsNumber (Document _ _ _ _ _ _ sections) = length sections



	-- OTHER GET FUNCTIONS
	getTitles :: Document -> [String]
	getTitles (Document _ _ _ _ _ _ sections) = getTitles' sections

	-- El argumento está compuesto por [[Titulo1,Seccion1], ..., [TituloN,SeccionN]] y devuelve [Titulo1, ..., TituloN]
	getTitles' :: [[String]] -> [String]
	getTitles' [[]] = []
	getTitles' (t:ts) = [head t] ++ getTitles' ts
	getTitles' _ = []



	-- RETURN ALL DOCUMENTS
	getDocuments :: [String] -> IO [Document]
	getDocuments (l:ls) = do
		x <- readDocument l
		xs <- getDocuments ls
		return $ [x] ++ xs
	getDocuments [] = return []


	-- PROCESS BUFFER OF DOCUMENTS
	filterByYear :: Int -> [Document] -> [Document]
	filterByYear year list = filter aux list
		where
			aux d = getYear d == year

	orderByTitle :: [Document] -> [Document]
	orderByTitle list = sort list