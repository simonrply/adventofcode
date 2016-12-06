module Main (main) where
import Data.List


valid :: Int -> Int -> Int ->  Bool
valid side1 side2 side3 
    | cond1 || cond2 || cond3 = False
    | otherwise = True 
    where 
        cond1 = side1 + side2 <= side3
        cond2 = side1 + side3 <= side2
        cond3 = side2 + side3 <= side1


file = "input.txt"

toSideList :: String -> [[Int]]
toSideList input = map lineToSideList  (lines input)  
    where lineToSideList line = map (\element -> read element :: Int) (words line) 

solution :: [[Int]] -> Int
solution list = foldl (\x (a:b:c:_) -> case (valid a b c) of
    True -> x + 1
    False -> x) 0 list

solution2 :: [Int] -> Int
solution2 [] = 0
solution2 list = case (valid a b c) of
                    True -> solution2 rest + 1
                    False -> solution2 rest
            where
                ((a:b:c:_),rest) = splitAt 3 list

transformList :: [[Int]] -> [Int]
transformList list = concat (transpose list)


main :: IO ()
main = do
    x <-  readFile file
    putStrLn $ show $ solution2 $ transformList $ toSideList  x
                                