evenCount1 :: [Int] -> Int
evenCount1 list = length $ filter(== 0) $ map(\x -> x `mod` 2) list

evenCount2 :: [Int] -> Int
evenCount2 list = -foldr (+) 0 (map(\x -> x `mod` 2 - 1) $ map(abs) list)

evenCount3 :: [Int] -> Int
evenCount3 list = length $ filter even list

main = do
     putStrLn(show $ evenCount1 [1,2,-4,8,1,-8,-3])
     putStrLn(show $ evenCount2 [1,2,-4,8,1,-8,-3])
     putStrLn(show $ evenCount3 [1,2,-4,8,1,-8,-3])