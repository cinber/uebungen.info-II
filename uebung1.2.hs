-- sort and search algorithms
-- @sam.berahman 2018

-- bubbleSort typ 1
bubbleSortList :: [Int] -> [Int]
bubbleSortList n =  case  bSortList n of
										m  | m == n    -> m
											 | otherwise -> bubbleSortList m
										where bSortList (x:x2:xs) | x > x2 = x2:(bSortList (x:xs))
																							| otherwise = x:(bSortList (x2:xs))
													bSortList n = n
-- bubbleSort typ 2
buSoList :: [Int] -> [Int]
buSoList [] = []
buSoList (x:x2:xs) =
		let sorted = if x2 < x
				then x2 : buSoList (x:xs)
				else x  : buSoList (x2:xs)
		in (buSoList $ init sorted) ++ [last sorted] -- $ kann ebenfalls durch Klammern ersetzt werden
buSoList xs = xs

-- check if element(int) is part of a list
containsList :: [Int] -> Int -> Bool
containsList [] _ = False
containsList (x:xs) element
		| element == x  = True
		| otherwise     = containsList xs element

-- count elements of a given list
import Data.Char (toLower)
countList :: [Char] -> Char -> Int
countList [] x = 0
countList  $ toLower (c:cs) $ toLower x  | x == c    = 1 + countList cs x
										| otherwise = countList cs x


-- x `elem` "aeiou" = "X" : censor xs
