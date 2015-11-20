module Papers(getPapers) where

	import System.Directory

	type Name = String
	type Data = String

	data FSItem = File Name Data | Folder Name [FSItem] deriving Show

	getPapers :: IO [String]
	getPapers = do
		actualPath <- getCurrentDirectory
		papersPath <- getDirectoryContents (actualPath ++ "/papersUTF8")
		return (addPath papersPath)

	addPath :: [String] -> [String] 
	addPath list = map ("papersUTF8/" ++) (clear list)

	clear :: [String] -> [String]
	clear list = filter (/=".") (filter (/="..") list)