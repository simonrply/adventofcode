module Main (main) where

file = "input.txt"

data Action = LeftAction | UpAction | RightAction | DownAction deriving (Show)

move :: Int -> Action -> Int
move pos LeftAction = case (pos) of
        _  | pos `elem` [1, 4, 7] 
            -> pos 
        _ -> pos - 1
move pos RightAction = case (pos) of
        _  | pos `elem` [3, 6, 9] 
            -> pos 
        _ -> pos + 1  
move pos UpAction = case (pos) of
        _  | pos `elem` [1, 2, 3] 
            -> pos 
        _ -> pos - 3
move pos DownAction = case (pos) of
        _  | pos `elem` [7, 8, 9] 
            -> pos 
        _ -> pos + 3

move2 :: Int -> Action -> Int
move2 pos LeftAction = case (pos) of
        _  | pos `elem` [1, 2, 5, 10, 13] 
            -> pos 
        _ -> pos - 1
move2 pos RightAction = case (pos) of
        _  | pos `elem` [1, 4, 9, 12, 13] 
            -> pos 
        _ -> pos + 1  
move2 pos UpAction = case (pos) of
        3 -> pos - 2 
        13 -> pos - 1
        _  | pos `elem` [1, 2, 4, 5, 9] 
            -> pos
           | otherwise
            -> pos - 4 
move2 pos DownAction = case (pos) of
        1  -> pos + 2
        11 -> pos + 2
        _  | pos `elem` [10, 12, 13, 5, 9] 
                -> pos
           | otherwise 
                -> pos + 4
    

toAction :: Char -> Action
toAction 'L' = LeftAction
toAction 'R' = RightAction
toAction 'U' = UpAction
toAction 'D' = DownAction

toActions :: String -> [[Action]]
toActions input = map (\list -> map toAction list) (lines input)

helper :: [Action] -> Int -> Int
helper list posStart = foldl (\pos action -> move2 pos action) posStart list

solution list = tail $ foldl (\x element -> x ++ [helper element (last x)]) [5] list


main :: IO ()
main = do
    x <-  readFile file
    putStrLn $ show $ solution $ toActions x
                                            