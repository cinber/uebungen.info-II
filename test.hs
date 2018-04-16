-- guard notation
nthElement :: [a] -> Int -> Maybe a
nthElement [] a  = Nothing
nthElement (x:xs) a | a <= 0 = Nothing
                    | a == 1 = Just x
                    | a > 1 = nthElement xs (a-1)

-- if-else notation
nthElementIf :: [a] -> Int -> Maybe a
nthElementIf [] a     = Nothing
nthElementIf (x:xs) a = if a <= 1
                          then if a <= 0
                                then Nothing
                              else Just x -- a == 1
                        else nthElementIf xs (a-1)

-- case notation
nthElementCases :: [a] -> Int -> Maybe a
nthElementCases [] a = Nothing
nthElementCases (x:xs) a = case a <= 0 of
                             True -> Nothing
                             False -> case a == 1 of
                                        True -> Just x
                                        False -> nthElementCases xs (a-1)


-- bubbleSort typ 1
bubbleSortList :: [Int] -> [Int]
bubbleSortList n =  case  bSortList n of
                    m  | m == n    -> m
                       | otherwise -> bubbleSortList m
                    where bSortList (x:x2:xs) | x > x2 = x2:(bSortList (x:xs))
                                              | otherwise = x:(bSortList (x2:xs))
                          bSortList n = n
-- bubbleSort typ 2
-- enthält einen logischen Fehler
buSoList :: [Int] -> [Int]
buSoList (c:c2:cs)
        | (c:c2:cs) == []   = []
        | c >= c2           = bSoList (c:c2:cs)
        | otherwise         = c:(buSoList (c2:cs))
      where bSoList (x:x2:xs) | x > x2 = x2:(bSoList (x:xs))
                              | otherwise = x:(bSoList (x2:xs))
            bSoList (c:c2:cs) = (c:c2:cs)
