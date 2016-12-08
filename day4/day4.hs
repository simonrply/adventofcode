module Main (main) where
import qualified Data.Map as Map
import Data.Char
import Data.List


data Room = Room String Int String
 
mostCommon :: String -> Map.Map Char Int
mostCommon name = foldl (\list char -> Map.insertWith (+)  char 1 list) Map.empty name
        
mostCommonFormated :: Map.Map Char Int -> [Char]
mostCommonFormated input = foldl (\list element -> list ++ [fst element] ) [] (take 5 $ sortOn f list)
        where
            list = Map.toList input
            f (char,number) = - (number * 100 - (ord char)) 

real :: Room -> Bool
real (Room name _ checksum) = (mostCommonFormated $ mostCommon name) == checksum  

  


main :: IO ()
main = return ()
