position :: Int -> [Int] -> Int
position _ [] = 0
position n (x:xs) | x == n = 1
                  | otherwise = position n xs + 1


main = do
      putStrLn("[1, 2, 3, 6, 8, 3, 3, 2, 10]")
      putStrLn("What number do you want to find?: ")
      n <- readLn
      putStrLn("This number has order number: ")
      putStrLn (show $ position n [1, 2, 3, 6, 8, 3, 3, 2, 10])