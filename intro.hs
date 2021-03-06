-- examples from lecture
-- @sam.berahman 2018

-- logarithm to base 2
log2 :: Float -> Float
log2 = logBase 2

-- euler number
e :: Float
e = exp 1

-- number*2
doubleMe :: Int -> Int
doubleMe x = x + x

-- double and add two numbers
doubleUs x y = doubleMe x + doubleMe y

-- doubleSmallNumber and add 1
doubleSmallNumber x = (if x > 100 then x else x*2) + 1

-- absolute
absolute :: Int -> Int
absolute x = if x < 0 then (-1)*x else x

-- compare two numbers
comp :: Int -> Int -> Int
comp x y
	| x > y 		= 1
	| x < y 		= -1
	| otherwise 	= 0

 -- XOR
(<+>) :: Bool -> Bool -> Bool
(<+>) a b
	| a /= b 		= True
	| otherwise 	= False

-- heron(quadratic)
heronB :: (Int, Double) -> Double
heronB (n, a)
	|n > 0		= (heronB ((n-1),a) + a / heronB ((n-1), a))/2
	|otherwise	= a
-- mit hilfe von "where"
heronC :: Int -> Double -> Double
heronC n a
	|n > 0		= (x + a / x)/2
	|otherwise	= a
	where x = heronC (n-1) a

-- heronD mit hilfe von Pattern Matching
heronD :: Int -> Double -> Double
heronD 0 a = a
heronD n a
	| n > 0	= (x + a / x) / 2
	where x = heronD (n-1) a


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


