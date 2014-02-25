import Data.Char

digitSum :: Int -> Int
digitSum 0 = 0
digitSum n | n > 0 = foldl (+) 0 $ map(\ch -> ord ch - ord '0') (show n)
           | otherwise = error "Negative argument"

main = do
      putStrLn("Enter number: ")
      n <- readLn
      putStrLn("Digit sum: ")
      putStrLn(show $ digitSum n)