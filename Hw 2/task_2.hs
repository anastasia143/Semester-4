degreesOfTwo :: Int -> [Int] -> [Int]
degreesOfTwo n xs | n > 0 = degreesOfTwo (n - 1) (2^n:xs)
                  | n == 0 = 1:xs
                  | otherwise = error("Negative argument")

main = do
      putStrLn("Enter degree: ")
      n <- readLn
      putStrLn("Result: ")
      putStrLn(show $degreesOfTwo n [])