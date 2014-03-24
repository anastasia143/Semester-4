-- FOR TASK (with accumulators)
calculate :: [Double] -> Double
calculate xs = (sum 0 xs) / (cosMult 1 xs)
      where sum s [] = s
            sum s (x:xs) = sum (s + x) xs
            cosMult m [] = m
            cosMult m (x:xs) = cosMult (cos(x) * m) xs
            
-- FOR CHECKING
add = foldr (+) 0
mult= foldr (*) 1 . map(cos)
result xs = add xs / mult xs

main = do
    putStr(show $ calculate [1,2,4])
    putStr " vs "
    putStrLn(show $ result [1,2,4])
    putStr(show $ calculate [3,7,8])
    putStr " vs "
    putStrLn(show $ result [3,7,8])