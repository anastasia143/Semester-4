import Data.Char
figures :: Int -> [Int]
figures n = map (\ch -> ord ch - ord '0') $ show n

next :: Int -> Int
next 1 = 7
next 7 = 9
next 9 = 11
next x | lastCh == 9 = 1 + next tenDiv * 10
       | otherwise = tenDiv * 10 + mod (next lastCh) 10
     where lastCh = head $ reverse $ figures x
           tenDiv = div x 10

list = 1: map next list

main = do
     putStrLn(show $ take 15 list)