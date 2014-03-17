sumPartition :: Int -> [[Int]]
sumPartition 0 = [[]]
sumPartition n = helper n n
     where helper n maxPart | n > 0 = [1..maxPart] >>= (\x -> map (x:) (helper (n - x) $ min x $ n - x))
                            | otherwise = [[]]

main = do
    putStrLn(show $ sumPartition 3)
    putStrLn(show $ sumPartition 4)
    putStrLn(show $ sumPartition 5)