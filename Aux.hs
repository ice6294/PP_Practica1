module Aux where

	import Data.List
	import Data.Ord
	import Documento
	import Acronimo
	import Extras



	-- DATAS
	data Extra = Extra
		{	doc	:: Document
		,	aux :: [Auxiliar]	}

	instance Eq Extra where
		a == b = (getDoc a) == (getDoc b)

	instance Ord Extra where
		compare a b = compare (getDoc a) (getDoc b)



	data Auxiliar = Auxiliar
		{	acronym	:: Acronym
		,	n		:: Integer	}

	instance Eq Auxiliar where
		a == b = (getN a) == (getN b)

	instance Ord Auxiliar where
		compare a b = compare (getN a) (getN b)

	-- GET METHODS
	getDoc :: Extra -> Document
	getDoc (Extra doc _) = doc

	getAux :: Extra -> [Auxiliar]
	getAux (Extra _ aux) = aux

	getAcronym :: Auxiliar -> Acronym
	getAcronym (Auxiliar acronym _) = acronym

	getN :: Auxiliar -> Integer
	getN (Auxiliar _ n) = n



	-- READ METHODS
	{- readExtras -> readExtra -> simplify (-> readAuxiliars) -> sortByAuxLength (-> compareAuxLenght)-}
	readExtras :: [Document] -> [Extra]
	readExtras [] = []
	readExtras (d:ds) = [readExtra d] ++ readExtras ds

	readExtra :: Document -> Extra
	readExtra doc = (Extra doc (simplify $ readAuxiliars $ sort $ getAcronyms doc))

	simplify :: [Auxiliar] -> [Auxiliar]
	simplify list = take n (reverse $ sort list)
		where
			n = truncate $ logBase 2 (fromIntegral (length list + 1))

	readAuxiliars :: [Acronym] -> [Auxiliar]
	readAuxiliars [] = []
	readAuxiliars list = readAuxiliars' 1 (head aux) (tail aux)
		where
			aux = sort list

	readAuxiliars' :: Integer -> Acronym -> [Acronym] -> [Auxiliar]
	readAuxiliars' n acr [] = [(Auxiliar acr n)]
	readAuxiliars' n acr (a:as)
		|	acr /= a	= [(Auxiliar acr n)] ++ readAuxiliars' 1 a as
		|	otherwise	= readAuxiliars' (n+1) acr as

	sortByAuxLength :: [Extra] -> [Extra]
	sortByAuxLength list = sortBy compareAuxLenght list

	compareAuxLenght :: Extra -> Extra -> Ordering
	compareAuxLenght x y
		|	length (getAux x) < length (getAux y)	= GT
		|	length (getAux x) > length (getAux y)	= LT
		|	otherwise								= EQ



	-- SHOW FUNCTIONS
	showExtras :: [Extra] -> String
	showExtras [] = ""
	showExtras (e:es) = "\n" ++ bar ++ "Doc: (" ++ (show $ getIdent $ getDoc e) ++ ") " ++  (getTitle $ getDoc e) ++
						"\nAcronimos escogidos: \n" ++ (showAuxiliars $ getAux e) ++ showExtras es

	showAuxiliars :: [Auxiliar] -> String
	showAuxiliars [] = ""
	showAuxiliars (a:as) = "\t· " ++ (getAcr $ getAcronym a) ++ " (" ++ (show $ getN a) ++ ")\n" ++ showAuxiliars as


	{-clustering :: [Document] -> Int -> [[Document]]
	culstering list = []-}