import Data.List

-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []


-- Problem 15
repli :: [a] -> Int -> [a]
repli list times = foldr (\x acc -> (innerReplicate times x) ++ acc) [] list
  where innerReplicate 0 _ = []
        innerReplicate x character = character:(innerReplicate (x-1) character) 

-- Problem 16
dropMe ::(Eq a) => [a] -> Int -> [a]
dropMe list number  = dropme_ list (number,number)
    where dropme_ [] _ = []
          dropme_ (x:xs) (number,counter) = if counter == 1 then dropme_ xs (number,number)
                                            else x:dropme_ xs (number,counter-1)

-- Problem 17 (Probably cheated on this)
split :: [a] -> Int -> ([a],[a])
split list number = let alpha = [list !! a | a <- [0..number-1]]
		        beta = [list !! b | b <- [(number)..((length list) - 1)]]
                    in (alpha,beta)

-- Problem 17 (reiteration)
split2 :: [a] -> Int -> ([a],[a])
split2 list number = foldl foldingFunction ([],[]) list
    where foldingFunction (l,m) x = if length l < number then (l++[x],m) else (l,m++[x])

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice list start end = let slice_ [] _ = []
                           slice_ (x:xs) counter = if counter >= start && counter <= end then x:(slice_ xs (counter+1))
                                                   else slice_ xs (counter+1)
	               in slice_ list 1

-- Problem 19 // gotta infestigate why I can use '.' between functions
tupleFlip :: (a,a) -> (a,a)
tupleFlip (a,b) = (b,a)

tupleMerge :: ([a],[a]) -> [a]
tupleMerge (x,y) = x++y

rotate :: [a] -> Int -> [a]
rotate list number = if number < 0 then tupleMerge $ tupleFlip $ split list (number+listLength)
                     else tupleMerge $ tupleFlip $ split list number
                     where listLength = length list

-- Problem 20 
removeAt :: Int -> [a] -> ([a],[a])
removeAt number  =  foldl foldingFunction ([],[])
    where foldingFunction (l,m) x 
            | length m == number-1 && null l  = (x:l,m)
            | otherwise = (l,m++[x])
