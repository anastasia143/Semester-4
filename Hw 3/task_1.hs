func1 :: Int -> [Int] -> [Int]
func2 :: Int -> [Int] -> [Int]

func1 x xs = map (\y -> y*x) xs
-- func1 x = map (\y -> y*x)
-- func1 x = map (*x)
func2 = map . (*)

 
main = do
      putStrLn(show $ func2 5 [1, 2, 1])