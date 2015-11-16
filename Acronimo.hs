module Acronimo where

	import System.IO
	import Data.Char
	import Data.List


	-- DATAS
	data Acronim = Acronim	-- acr: acronimo, exp: forma expandida
		{	acr	:: String
		,	exp	:: String
		,	pos	:: Int		} deriving (Show)

	instance Eq Acronim where
		a == b = (getAcr a) == (getAcr b)

	instance Ord Acronim where
		compare a b = compare (getAcr a) (getAcr b)



	-- GET FUNCTIONS
	getAcr :: Acronim -> String
	getAcr (Acronim acr _ _) = acr

	getExp :: Acronim -> String
	getExp (Acronim _ exp _) = exp

	getPos :: Acronim -> Int
	getPos (Acronim _ _ pos) = pos



	-- READ ACRONIMS FROM REVERSED TEXT
	quickSearch :: String -> IO [Acronim]
	quickSearch text = do
		acronims <- quickSearch' (reverse text) 0
		return acronims

	quickSearch' :: String -> Int -> IO [Acronim]
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
						n2 <- endAcronim text n
						-- Si es demasiado largo
						if (n2-n) > 4 then
							do
								acronims <- quickSearch' text (n2+1)
								return $ acronims
						else
							do
								acr <- takeAcronim text n n2
								acronims <- quickSearch' text (n2+1)
								return $ acronims ++ [Acronim acr "" (length text - n)]
				-- Es una mayúscula aislada
				else
					do
						acronims <- quickSearch' text (n+1)
						return acronims
			-- No es mayúscula
			else
				do
					acronims <- quickSearch' text (n+1)
					return acronims

	searchAcronims :: String -> IO [Acronim]
	searchAcronims text = do
		acronims <- searchAcronims' (reverse text) 0
		return acronims

	searchAcronims' :: String -> Int -> IO [Acronim]
	searchAcronims' text n =
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
						n2 <- endAcronim text n
						-- Si es demasiado largo
						if (n2-n) > 4 then
							do
								acronims <- searchAcronims' text (n2+1)
								return $ acronims
						else
							do
								acr <- takeAcronim text n n2
								-- Intentamos recoger el significado
								exp <- takeExpanded acr text (length acr - 1) (n2+1)
								acronims <- searchAcronims' text (n2+1)
								return $ acronims ++ [Acronim acr exp (length text - n)]
				-- Es una mayúscula aislada
				else
					do
						acronims <- searchAcronims' text (n+1)
						return acronims
			-- No es mayúscula
			else
				do
					acronims <- searchAcronims' text (n+1)
					return acronims



	-- ACR
	isValid :: Char -> Bool
	isValid c = (isUpper c || isNumber c || c == '-') -- Problema con los numeros y con "-"

	endAcronim :: String -> Int -> IO (Int)
	endAcronim text n =
		if isValid $ text !! n then
			do
				n2 <- endAcronim text (n+1)
				return n2
		else
			if (text !! (n-1) == '-') then
				return $ n-2
			else
				return $ n-1

	takeAcronim :: String -> Int -> Int -> IO (String)
	takeAcronim text n n2 =
		if (n-1) == n2 then
			return ""
		else
			do
				acr <- takeAcronim text n (n2-1)
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



	-- SHOW FUNCTIONS
	showAcronim :: Acronim -> String
	showAcronim (Acronim acr "" _) = acr ++ "\n"
	showAcronim acronim = getAcr acronim ++ " -> " ++ getExp acronim ++ "\n"

	showAcronims :: [Acronim] -> String
	showAcronims [] = ""
	showAcronims (a:as) = "\t- " ++ showAcronim a ++ showAcronims as



	-- OTHER FUNCTIONS
	removeDuplicated :: [Acronim] -> [Acronim]
	removeDuplicated [] = []
	removeDuplicated list = nub $ removeDuplicated' (head aux) (tail aux)
		where
			aux = sort list

	removeDuplicated' :: Acronim -> [Acronim] -> [Acronim]
	removeDuplicated' acr [] = [acr]
	removeDuplicated' acr (a:as)
		|	acr /= a			= [acr] ++ removeDuplicated' a as
		|	getExp acr == ""	= [a]   ++ removeDuplicated' a as
		|	otherwise			= [acr] ++ removeDuplicated' acr as



	countDuplicates :: [Acronim] -> [String]
	countDuplicates [] = []
	countDuplicates list = countDuplicates' 1 (head aux) (tail aux)
		where
			aux = sort list

	countDuplicates' :: Int -> Acronim -> [Acronim] -> [String]
	countDuplicates' n acr [] = [(getAcr acr) ++ " -> (" ++ (show n) ++ ")"]
	countDuplicates' n acr (a:as)
		|	acr /= a	= [(getAcr acr) ++ " -> (" ++ (show n) ++ ")"] ++ countDuplicates' 1 a as
		|	otherwise	= countDuplicates' (n+1) acr as