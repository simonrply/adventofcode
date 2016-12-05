module Main (main) where
import Data.List

file = "input.txt"

data Action = LeftAction Int | RightAction Int
data Direction = North | South | East | West

transform :: String -> [Action]
transform input = case (elemIndex ',' input) of
    Just index -> [toAction (take index input)] ++ (transform $ drop (index + 2) input)
    Nothing -> [toAction input]

toAction :: String -> Action
toAction input = case (head input)  of
    'L' -> LeftAction $ number input
    'R' -> RightAction $ number input 
    where number input = read $ tail $ input :: Int


solution :: [Action] -> Int
solution input = abs a +  abs b
            where 
                (a, b, _) = foldl (\(x, y, direction) action ->  case (direction, action) of
                                        (North, LeftAction displacement)  -> (x - displacement, y, West)
                                        (South, RightAction displacement) -> (x - displacement, y, West)
                                        (North, RightAction displacement) -> (x + displacement, y, East)
                                        (South, LeftAction displacement)  -> (x + displacement, y, East)
                                        (East, LeftAction displacement)   -> (x , y + displacement, North)
                                        (West, RightAction displacement)  -> (x , y + displacement, North)
                                        (East, RightAction displacement)  -> (x, y - displacement, South)
                                        (West, LeftAction displacement)   -> (x, y - displacement, South)  
                                  )(0,0,North) input



main :: IO ()
main = do
    x <- readFile file
    putStrLn $ show $ solution $ transform x
