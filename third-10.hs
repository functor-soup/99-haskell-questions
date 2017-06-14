import System.Random 
import Data.List

-- Problem 21 
insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt x container index  = let (first,second) = splitAt index container
                              in first ++ [x] ++ second

-- Problem 22
range :: Int -> Int -> [Int]
range a b = let recFunction x
                   | x <= b = x:recFunction (x + 1)
                   | otherwise = []
            in recFunction a

-- Problem 23
import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select y x = map (y !!) . take x . randomRs (0, (length y) - 1) <$> newStdGen
                    

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




