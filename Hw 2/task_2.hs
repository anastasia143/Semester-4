degreesOfTwo :: Int -> [Int]
degreesOfTwo 0 = [1]
degreesOfTwo n | n > 0 = (degreesOfTwo (n - 1)) ++ [2^n]
               | otherwise = error("Negative argument")

main = do
      putStrLn("Enter degree: ")
      n <- readLn
      putStrLn("Result: ")
      putStrLn(show $degreesOfTwo n)