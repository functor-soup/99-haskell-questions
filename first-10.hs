
-- Problem 1 
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- Problem 2
mySecondLast :: [a] -> Maybe a
mySecondLast [] = Nothing
mySecondLast [x] = Nothing
mySecondLast [x,y] = Just x
mySecondLast (x:xs) = mySecondLast xs

-- Problem 3
myIndex :: Int -> [a] -> Maybe a
myIndex _ [] = Nothing
myIndex 0 (x:xs) = Just x 
myIndex index (x:xs) = myIndex (index-1) xs

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc element ->  element:acc) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == myReverse list

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x)  = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs)) 

-- Problem 8
myCompress :: (Eq a) => [a] -> [a]
myCompress = foldr (\x acc -> if (null acc || x /= head acc ) then x:acc else acc) []

-- Problem 10
myPackCompress ::(Eq a) => [a] -> [(Int,a)]
myPackCompress = foldr foldingFunction []
	where foldingFunction x [] = [(1,x)]
	      foldingFunction a l@((x,b):xs) = if b /= a then (1,a):l
					   else ((x+1,b):xs) 
-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack = map (\(x,y) -> take x $ repeat y ).myPackCompress 
