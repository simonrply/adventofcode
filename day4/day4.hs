module Main (main) where
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe

data Room = Room String Int String deriving (Show)


lineToRoom :: String -> Room
lineToRoom input = Room name sectorId checksum
    where 
        indexCheck = fromJust $ elemIndex '[' input
        sectorId = read $ drop (indexCheck - 3) $ take indexCheck input :: Int 
        checksum = init $ drop (indexCheck + 1) input
        name = (take (indexCheck - 4) input)


linesToRoomList :: String -> [Room]
linesToRoomList input = map lineToRoom (lines input)

solution :: [Room] -> Int
solution list = foldl (\x (Room name roomId checksum) -> case (real name checksum) of
    True -> x + roomId
    False -> x) 0 list
 
mostCommon :: String -> Map.Map Char Int
mostCommon name = foldl (\list char -> case char of
    '-' -> list
    _ -> Map.insertWith (+)  char 1 list) Map.empty name
        
mostCommonFormated :: Map.Map Char Int -> [Char]
mostCommonFormated input = foldl (\list element -> list ++ [fst element] ) [] (take 5 $ sortOn f list)
        where
            list = Map.toList input
            f (char,number) = - (number * 100 - (ord char)) 

real :: String -> String -> Bool
real name checksum = (mostCommonFormated $ mostCommon name) == checksum  


rotate :: Char -> Char
rotate '-' = ' '
rotate ' ' = ' '  
rotate char = chr ((((ord char) - 97 + 1) `mod` 26) + 97) 
    
rotateX :: Int -> Char -> Char
rotateX 0 char = char
rotateX x char = rotateX (x - 1) (rotate char)

decryptName :: Room -> Room
decryptName (Room name sectorId checksum) = Room (map (\element -> rotateX sectorId element) name) sectorId checksum


file = "input.txt"

main :: IO ()
main =  do
    x <-  readFile file
    putStrLn $ show $ solution $ linesToRoomList  x
                                