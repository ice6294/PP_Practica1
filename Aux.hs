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
		,	n		:: Int	}

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

	getN :: Auxiliar -> Int
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

	readAuxiliars' :: Int -> Acronym -> [Acronym] -> [Auxiliar]
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



	-- CLUSTERING
	-- Clustering muestra por pantalla las agrupaciones de documentos semejantes
	clustering :: [Extra] -> IO ()
	clustering exts = putStrLn $ showClusters $ removeEmptyClusters $ clustering' exts 0

	-- Clustering' recibe los documentos, la posición actual, la ronda que toca, y devuelve un String
	clustering' :: [Extra] -> Int -> [[Extra]]
	clustering' exts i
		-- Si estamos en medio de una ronda, comparamos el documento i con sus siguientes (roundCompare)
		|	i < length exts	=	let x = roundCompare (exts !! i) (drop (i+1) exts) in
								-- Si no encuentra parecidos, se sigue normalmente con el clustering'
								if (length x == 1) then
									clustering' exts (i+1)
								else
									[x] ++ clustering' (filterExtras exts x) i

		-- Si hemos termiando la ronda se meten todos al saco
		|	otherwise		=	clusteringRest exts

	clusteringRest :: [Extra] -> [[Extra]]
	clusteringRest [] = []
	clusteringRest (e:es) = [[e]] ++ clusteringRest es

	-- roundCompare recibe el documento actual, los siguientes docuentos y la ronda, y compara el documento con sus siguientes
	roundCompare :: Extra -> [Extra] -> [Extra]
	roundCompare ext [] = [ext]
	roundCompare ext (e:es) =
		if (compareAux ext e) then
			[e] ++ roundCompare ext es
		else
			roundCompare ext es

	-- comparamos el acrónimo más usado de ambos
	compareAux :: Extra -> Extra -> Bool
	compareAux e1 e2 =
		if (((length $ getAux e1) > 0) && ((length $ getAux e2) > 0)) then
			(getAcronym $ head $ getAux $ e1) == (getAcronym $ head $ getAux $ e2)
		else
			False

	-- Devolvemos una lista "c" que contiene los documentos de "a" que no están en "b"
	filterExtras :: [Extra] -> [Extra] -> [Extra]
	filterExtras [] _ = []
	filterExtras (a:as) b
		|	elem a b	= filterExtras as b
		|	otherwise	= [a] ++ filterExtras as b

	removeEmptyClusters :: [[Extra]] -> [[Extra]]
	removeEmptyClusters [] = []
	removeEmptyClusters (e:es) =
		if (e == []) then
			removeEmptyClusters es
		else
			[e] ++ removeEmptyClusters es



	-- SHOW FUNCTIONS
	showExtras :: [Extra] -> String
	showExtras [] = ""
	showExtras (e:es) = "\n" ++ bar ++ "Doc: (" ++ (show $ getIdent $ getDoc e) ++ ") " ++  (getTitle $ getDoc e) ++
						"\nAcronimos escogidos: \n" ++ (showAuxiliars $ getAux e) ++ showExtras es

	showAuxiliars :: [Auxiliar] -> String
	showAuxiliars [] = ""
	showAuxiliars (a:as) =	"\t· " ++ (getAcr $ getAcronym a) ++ " (" ++ (show $ getN a) ++ ")" ++
							"\t" ++ (getExp $ getAcronym a) ++ "\n" ++ showAuxiliars as



	-- SHOW CLUSTERS FUNCTIONS
	showClusters :: [[Extra]] -> String
	showClusters exts = showClusters' exts 0

	showClusters' :: [[Extra]] -> Int -> String
	showClusters' [] _ = ""
	showClusters' (e:es) i = bar ++ "Cluster " ++ (show i) ++ 
							(showAcrCluster e) ++ "\n" ++ (showCluster e) ++ showClusters' es (i+1)

	showAcrCluster :: [Extra] -> String
	showAcrCluster [] = ""
	showAcrCluster (e:_) =
		if ((length $ getAux e) > 0) then
			" (related to \"" ++ (getAcr $ getAcronym $ head $ getAux e) ++ "\")"
		else
			""

	showCluster :: [Extra] -> String
	showCluster [] = ""
	showCluster (e:es) = "\t· " ++ (show $ getIdent $ getDoc e) ++ " - " ++ (getTitle $ getDoc e) ++ "\n" ++ (showCluster es)