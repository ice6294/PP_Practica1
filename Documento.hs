module Documento where

	import System.IO
	import System.IO.Unsafe
	import Control.Monad
	import Papers

	-- type Section :: String

{-
	data Acronim = Acronim
		{	short 		:: String
		,	expanded	:: String }
-}

	data Document = Document
		{	title			:: String
		,	ident			:: Int
		,	year			:: Int
		{-,	abstract		:: String
		,	section_number	:: Int 
		,	section 		:: [Section]
		,	acronims		:: [Acronim]-} } deriving (Show)

	instance Eq Document where
		a == b = (getTitle a) == (getTitle b)

	instance Ord Document where
		compare a b = compare (getTitle a) (getTitle b)

	readDocument :: String -> IO Document
	readDocument path = do
		handle <- openFile path ReadMode
		title <- hGetLine handle
		ident <- hGetLine handle
		year <- hGetLine handle
		hClose handle
		return (Document title (read ident::Int) (read year::Int))

	-- Show functions
	showAllDocuments :: [Document] -> String
	showAllDocuments (d:ds) = showDocument d ++ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" ++ showAll ds
	showAll [] = ""

	showDocument :: Document -> String
	showDocument doc = getTitle doc ++ "\n" ++ (show $ getIdent doc) ++ "\n" ++ (show $ getYear doc)

	getTitle :: Document -> String
	getTitle (Document title _ _) = title

	getIdent :: Document -> Int
	getIdent (Document _ ident _) = ident

	getYear :: Document -> Int
	getYear (Document _ _ year) = year

	-- Get from
	getDocuments :: [String] -> [Document]
	getDocuments lista = map (aux) lista
		where
			aux d = unsafePerformIO (readDocument d)

	funAuxiliar :: IO [Document]
	funAuxiliar = do
		cosa <- getPapers
		cosa2 <- getDocuments cosa
		return cosa2