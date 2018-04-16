-- mainly mathematical algorithms
-- @sam.berahman 2018

-- quadratic equations
quadratic :: (Double, Double, Double) -> (Double, Double)
quadratic (a, b, c) = (x1, x2)
	where
		x1 	= p + sqrt q / (2 * a)
		x2	= p - sqrt q / (2 * a)
		q 	= b * b - 4 * a * c
		p 	= - b / (2 * a)

-- square number
square :: Int -> Int
square n
	| n < 0   		= square(-n)
	| n == 0  		= 0
	| otherwise 	= 2 * n - 1 + square(n-1)

-- sum of a list
sumList :: [Int] -> Int
sumList [] 			= 0
sumList (x:xs) 	= x + (sumList xs)

-- foldList
foldList :: (Double -> Double -> Double) -> [Double] -> Double
foldList f [x]       = x
foldList f [x,y]     = f x y
foldList f (x:y:zs)  = f (f x y) (foldList f zs)
foldList _ []        = error "foldList undefined on empty []"

-- mapList
mapList :: (Int -> Int) -> [Int] -> [Int]
mapList f [] 			= []
mapList f (x:xs) 	= f x : mapList f xs
-- die Eingabe von quadratic funktioniert nicht

-- test function
tableInt :: (Int -> Int) -> [Int] -> String
tableInt f xs
  	| xs == [] = ""
		| otherwise = show (head xs) 	++ " : " ++ show (f(head xs)) ++ "\n" ++
										tableInt f (tail xs)
