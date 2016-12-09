module Main (main) where


import Data.List

mostCommon :: String -> Char
mostCommon list = head . last . sortOn length . group $ (sort list)


file = "input.txt"

main :: IO ()
main =  do
    x <-  readFile file
    putStrLn $ foldl (\x element -> x ++ [mostCommon element]) "" (transpose (lines x))

                    