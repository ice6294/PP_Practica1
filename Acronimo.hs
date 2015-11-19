module Acronimo where

	import System.IO
	import Data.Char
	import Data.List

	-- DATAS
	data Acronym = Acronym	-- acr: acronimo, exp: forma expandida
		{	acr	:: String
		,	exp	:: String
		,	pos	:: Int		} deriving (Show)

	instance Eq Acronym where
		a == b = (getAcr a) == (getAcr b)

	instance Ord Acronym where
		compare a b = compare (getAcr a) (getAcr b)



	-- GET FUNCTIONS
	getAcr :: Acronym -> String
	getAcr (Acronym acr _ _) = acr

	getExp :: Acronym -> String
	getExp (Acronym _ exp _) = exp

	getPos :: Acronym -> Int
	getPos (Acronym _ _ pos) = pos



	-- READ ACRONIMS FROM REVERSED TEXT
	quickSearch :: String -> IO [Acronym]
	quickSearch text = do
		acronyms <- quickSearch' (reverse text) 0
		return acronyms

	quickSearch' :: String -> Int -> IO [Acronym]
	quickSearch' text n =
		-- Si hemos llegado al final del texto
		if n == (length text - 1) then
			return []
		else
			-- Si es mayúscula
			if isUpper $ text !! n then
				-- Si es Acrónimo (es decir, no es mayúscula aislada)
				if isUpper $ text !! (n+1) then
					do
						-- Recogemos el acrónimo
						n2 <- endAcronym text n
						-- Si es demasiado largo
						if (n2-n) > 4 then
							do
								acronyms <- quickSearch' text (n2+1)
								return $ acronyms
						else
							do
								acr <- takeAcronym text n n2
								acronyms <- quickSearch' text (n2+1)
								return $ acronyms ++ [Acronym acr "" (length text - n)]
				-- Es una mayúscula aislada
				else
					do
						acronyms <- quickSearch' text (n+1)
						return acronyms
			-- No es mayúscula
			else
				do
					acronyms <- quickSearch' text (n+1)
					return acronyms

	searchAcronyms :: String -> IO [Acronym]
	searchAcronyms text = do
		acronyms <- searchAcronyms' (reverse text) 0
		return acronyms

	searchAcronyms' :: String -> Int -> IO [Acronym]
	searchAcronyms' text n =
		-- Si hemos llegado al final del texto
		if n == (length text - 1) then
			return []
		else
			-- Si es mayúscula
			if isUpper $ text !! n then
				-- Si es Acrónimo (es decir, no es mayúscula aislada)
				if isUpper $ text !! (n+1) then
					do
						-- Recogemos el acrónimo
						n2 <- endAcronym text n
						-- Si es demasiado largo
						if (n2-n) > 4 then
							do
								acronyms <- searchAcronyms' text (n2+1)
								return $ acronyms
						else
							do
								acr <- takeAcronym text n n2
								-- Intentamos recoger el significado
								exp <- takeExpanded acr text (length acr - 1) (n2+1)
								acronyms <- searchAcronyms' text (n2+1)
								return $ acronyms ++ [Acronym acr exp (length text - n)]
				-- Es una mayúscula aislada
				else
					do
						acronyms <- searchAcronyms' text (n+1)
						return acronyms
			-- No es mayúscula
			else
				do
					acronyms <- searchAcronyms' text (n+1)
					return acronyms



	-- ACR
	isValid :: Char -> Bool
	isValid c = (isUpper c || isNumber c || c == '-') -- Problema con los numeros y con "-"

	endAcronym :: String -> Int -> IO (Int)
	endAcronym text n =
		if isValid $ text !! n then
			do
				n2 <- endAcronym text (n+1)
				return n2
		else
			if (text !! (n-1) == '-') then
				return $ n-2
			else
				return $ n-1

	takeAcronym :: String -> Int -> Int -> IO (String)
	takeAcronym text n n2 =
		if (n-1) == n2 then
			return ""
		else
			do
				acr <- takeAcronym text n (n2-1)
				return $ [text !! n2] ++ acr



	-- EXP
	-- devuelve String entero de la forma extendida
	{- takeExpanded   acr   ->  text  ->n_acr->n_text->   exp	| n_acr: length acronim, n_text: beginin rest (in text)-}
	takeExpanded :: String  -> String -> Int -> Int -> IO String
	takeExpanded acr text n_acr n_text =
		if (text !! n_text == '(') && (text !! (n_text - n_acr - 2) == ')') then
			do
				exp <- takeExpWords text (n_text + 1) (endExpanded acr text n_acr (n_text+1))
				if (length exp < 60)
					then return exp
					else return ""
		else
			do
				return ""

	-- match der-izq, devuelve la posición de la letra final de la forma expandida
	{- endExpanded   acr  ->  text  ->n_acr->n_text-> end		| n_acr: length acronim, n_text: beginin rest (in text)-}
	endExpanded :: String -> String -> Int -> Int -> Int
	endExpanded acr text (-1) n_text = n_text				-- LA ÚLTIMA TIENE QUE IR PROCEDIDA DE UN ESPACIO!!!!
	endExpanded acr text n_acr n_text =
		-- Si el acronimo contiene "-" se salta
		if ((acr !! n_acr == '-') || (isDigit $ acr !! n_acr)) then
			endExpanded acr text (n_acr-1) n_text
		else
			-- Si se ha encontrado una letra se tacha y se sigue buscando
			if (toUpper (text !! n_text) == (acr !! n_acr)) then
				endExpanded acr text (n_acr-1) (n_text+1)
			-- Si no se ha encontrado la siguiente letra se sigue buscando
			else
				endExpanded acr text n_acr (n_text+1)

	-- recoge todas las palabras hasta el caracter final de la forma expandida | n: begin, n2: end
	takeExpWords :: String -> Int -> Int -> IO String
	takeExpWords text n n2 =
		if (n == n2) then
			do
				exp <- takeExpWords' text n
				return exp
		else
			do
				exp <- takeExpWords text (n+1) n2
				return $ exp ++ [text !! n]

	-- si queda algo por coger de la última palabra, se coge
	takeExpWords' :: String -> Int -> IO String
	takeExpWords' text n =
		if (isLetter (text !! n)) then
			do
				exp <- takeExpWords' text (n+1)
				return $ exp ++ [text !! n]
		else
			return ""

	-- INTENTO DE ARREGLO: LEER PRIMER CARACTER DE LA VERSIÓN EXPANDIDA
	{-takeExpWords'' :: String -> Int -> IO String
	takeExpWords'' text n =
		if (isLetter (text !! n)) then
			do
				exp <- takeExpWords'' text (n+1)
				return $ exp ++ [text !! n]
		else
			return ""-}



	-- SHOW FUNCTIONS
	showAcronym :: Acronym -> String
	showAcronym (Acronym acr "" _) = acr ++ "\n"
	showAcronym acronym = getAcr acronym ++ " -> " ++ getExp acronym ++ "\n"

	showAcronyms :: [Acronym] -> String
	showAcronyms [] = ""
	showAcronyms (a:as) = "\t- " ++ showAcronym a ++ showAcronyms as



	-- OTHER FUNCTIONS
	removeDuplicates :: [Acronym] -> [Acronym]
	removeDuplicates [] = []
	removeDuplicates list = nub $ removeDuplicates' (head aux) (tail aux)
		where
			aux = sort list

	removeDuplicates' :: Acronym -> [Acronym] -> [Acronym]
	removeDuplicates' acr [] = [acr]
	removeDuplicates' acr (a:as)
		|	acr /= a			= [acr] ++ removeDuplicates' a as
		|	getExp acr == ""	= [a]   ++ removeDuplicates' a as
		|	otherwise			= [acr] ++ removeDuplicates' acr as



	countDuplicates :: [Acronym] -> [String]
	countDuplicates [] = []
	countDuplicates list = countDuplicates' 1 (head aux) (tail aux)
		where
			aux = sort list

	countDuplicates' :: Int -> Acronym -> [Acronym] -> [String]
	countDuplicates' n acr [] = [(getAcr acr) ++ " -> (" ++ (show n) ++ ")"]
	countDuplicates' n acr (a:as)
		|	acr /= a	= [(getAcr acr) ++ " -> (" ++ (show n) ++ ")"] ++ countDuplicates' 1 a as
		|	otherwise	= countDuplicates' (n+1) acr as