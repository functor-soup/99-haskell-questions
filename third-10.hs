 -- Problem 21 

-- Problem 21 
insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt x someString someNumber  = let zippedList = zip [1..] someString
                                        foldingFunction (index,a) acc 
                                                | index == someNumber = (index,x):(index,a):acc
                                                | otherwise = (index,a):acc
                                        extractorFunction = foldr (\(_,a) acc -> a:acc) []
                                    in extractorFunction $ foldr foldingFunction [] zippedList

-- Problem 22
range :: Int -> Int -> [Int]
range a b = let recFunction x
                   | x <= b = x:recFunction (x + 1)
                   | otherwise = []
            in recFunction a

-- to be continued after I learn how to deal with random number generators in haskell
