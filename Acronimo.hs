module Acronimo where

	import System.IO
	import Data.Char


	-- DATAS
	data Acronim = Acronim	-- acr: acronimo, exp: forma expandida
		{	acr	:: String
		,	exp	:: String } deriving (Show)



	-- GET FUNCTIONS
	getAcr :: Acronim -> String
	getAcr (Acronim acr _) = acr

	getExp :: Acronim -> String
	getExp (Acronim _ exp) = exp



	-- READ ACRONIMS FROM REVERSED TEXT
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
			if isValid $ text !! n then
				-- Si es Acrónimo (es decir, no es mayúscula aislada)
				if isValid $ text !! (n+1) then
					do
						-- Recogemos el acrónimo
						n2 <- endAcronim text n
						acr <- takeAcronim text n n2
						putStrLn $ "ENCONTRADO! -> " ++ acr
						-- Intentamos recoger el significado
						putStr "Intentamos buscar su significado con: "
						putStrLn $ "takeExpanded " ++ acr ++ " text " ++ (show $ length acr - 1) ++ " " ++ (show (n2+1))
						putStrLn $ "Empieza buscando la " ++ [acr !! (length acr - 1)] ++ " por la " ++ [text !! (n2 + 1)]

						exp <- takeExpanded acr text (length acr - 1) (n2+1)
						putStrLn $ "Significado de " ++ acr ++ " : " ++ exp
						acronims <- searchAcronims' text (n2+1)
						return $ acronims ++ [Acronim acr exp]
				-- Es una mayúscula aislada
				else
					do
						print ("Es mayuscula aislada: " ++ [text !! n])
						acronims <- searchAcronims' text (n+1)
						return acronims
			-- No es mayúscula
			else
				do
					acronims <- searchAcronims' text (n+1)
					return acronims



	-- ACR
	isValid :: Char -> Bool
	isValid c = (isUpper c || isNumber c || c == '-')

	endAcronim :: String -> Int -> IO (Int)
	endAcronim text n =
		if isValid $ text !! n then
			do
				n2 <- endAcronim text (n+1)
				return n2
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
	{- takeExpanded   acr   ->  text  ->n_acr->n_text->   exp	| n_acr: lenght acronim, n_text: beginin rest (in text)-}
	takeExpanded :: String  -> String -> Int -> Int -> IO String
	takeExpanded acr text n_acr n_text =
		if (text !! n_text == '(') then
			do
				putStr $ "\t$" ++ acr ++ " -> Con significado -> "
				putStrLn $ "takeExpWords text " ++ (show $ n_text + 1) ++ " (endExpanded " ++ acr ++ " text " ++ (show n_acr) ++ " " ++ (show $ n_text+1) ++ ")" ++ " ~~~~~~~ endExpanded = " ++ (show $ endExpanded acr text n_acr (n_text+1))
				exp <- takeExpWords text (n_text + 1) (endExpanded acr text n_acr (n_text+1))
				return exp
		else
			do
				putStrLn $ "\t#" ++ acr ++ " -> Sin Significado. Pasamos a otra ..."
				return ""

	-- match der-izq, devuelve la posición de la letra final de la forma expandida
	{- endExpanded   acr  ->  text  ->n_acr->n_text-> end		| n_acr: lenght acronim, n_text: beginin rest (in text)-}
	endExpanded :: String -> String -> Int -> Int -> Int
	endExpanded _ _ 0 n_text = n_text		-- Llamar a funcion que termine de recoger la palabra
	endExpanded acr text n_acr n_text =
		-- Si el acronimo contiene "-" se salta
		if (acr !! n_acr == '-') then
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
	showAcronim acronim = "Acronimo: " ++ getAcr acronim ++ ". Significado: " ++ getExp acronim ++ "\n"

	showAcronims :: [Acronim] -> String
	showAcronims [] = ""
	showAcronims (a:as) = showAcronim a ++ showAcronims as










	textoPrueba = "Acá van algunos ejemplos de acronimos. AA-1 seria uno sin significado, blue basketball (BBB) sería otro, pero C no lo debería ser. Tal vez djeis for hallk suju (DJFHKSJ) si, esperemos jajaja. No."

	prueba :: IO ()
	prueba = do
		all <- searchAcronims textoPrueba
		putStrLn ""
		putStrLn $ showAcronims all