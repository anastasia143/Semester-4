list :: Int -> [Int]
list 1 = [1]
list n = [1..n] >>= (\x -> map(x*) [1..n])

main = do
    putStrLn(show $ list 2)
    putStrLn(show $ list 3)
    putStrLn(show $ list 4)