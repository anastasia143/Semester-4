import Data.Char

numbers :: Int -> [Int]
numbers n = map (\ch -> ord ch - ord '0') $ show n

check :: [Int] -> Bool
check [] = True
check (n:ns) | (n == 1 || n == 7 || n == 9) = check(ns)
             | otherwise = False
             
list :: [Int]
list = [x | x <- [1..], check $ numbers x]

main = do
    putStrLn(show $ take 13 list)