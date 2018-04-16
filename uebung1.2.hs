-- sort and search algorithms
-- @sam.berahman 2018

-- check if element(int) is part of a list
containsList :: [Int] -> Int -> Bool
containsList [] _ = False
containsList (x:xs) element
		| element == x  = True
		| otherwise     = containsList xs element

-- count elements of a given list
import Data.Char (toLower)
countList :: [Char] -> Char -> Int
countList [] x 		= 0
countList [c] x 	= (if(toLower c) == (toLower x) then 1 else 0)
countList (c:cs) b = (if(toLower c) == (toLower x) then 1 else 0) + (countList xs b)
