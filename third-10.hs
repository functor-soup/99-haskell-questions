import System.Random 
import Data.List

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

-- Problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select list number = do
		    gen <- newStdGen
                    let newList = take number $ randomRs (0,(length list)-1) gen
                    return [list !! n | n <- newList]
                    

-- Problem 25  I can't believe I got this to work .. sniff I would like to thank applicative functors and my mom 
rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu [] = return []
rnd_permu [x] = return  [x]
rnd_permu l@(x:xs) = do
                     gen <- newStdGen
                     let (randomIndex,_) = randomR (0,(length l)-1) gen                 
                         randomElement = l !! randomIndex
                         restOfTheList = l \\ [randomElement]
                     (randomElement:) <$> (rnd_permu restOfTheList)




