module Documento where

	import System.IO
	import System.IO.Unsafe
	import Data.List
	import Control.Monad
	import Papers


	-- SCALARS
	bar = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"


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
		{-,	abstract		:: String
		,	section_number	:: Int 
		,	section 		:: [Section]
		,	acronims		:: [Acronim]-} } deriving (Show)

	instance Eq Document where
		a == b = (getTitle a) == (getTitle b)

	instance Ord Document where
		compare a b = compare (getTitle a) (getTitle b)


	-- READ ONE DOCUMENT
	readDocument :: String -> IO Document
	readDocument path = do
		handle <- openFile path ReadMode
		journal <- hGetLine handle		-- Revista
		ident <- hGetLine handle		-- ID
		year <- hGetLine handle			-- Año
		no <- hGetLine handle 			-- (--)
		title <- hGetLine handle		-- Título
		no <- hGetLine handle			-- (--)
		hClose handle
		return (Document path journal (read ident::Int) (read year::Int) title)


	-- SHOW FUNCTIONS
	showAllDocuments :: [Document] -> String
	showAllDocuments (d:ds) = showDocument d ++ "\n" ++ showAllDocuments ds
	showAllDocuments [] = ""

	showDocument :: Document -> String
	showDocument doc = "Title: " ++ getTitle doc ++ " (" ++ (show $ getYear doc) ++ ")\n"


	-- GET FUNCTIONS
	getPath :: Document -> String
	getPath (Document path _ _ _ _) = path

	getJournal :: Document -> String
	getJournal (Document _ journal _ _ _) = journal

	getIdent :: Document -> Int
	getIdent (Document _ _ ident _ _) = ident

	getYear :: Document -> Int
	getYear (Document _ _ _ year _) = year

	getTitle :: Document -> String
	getTitle (Document _ _ _ _ title) = title


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