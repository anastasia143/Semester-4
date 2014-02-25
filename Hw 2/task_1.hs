rev :: [a] -> [a]
rev [] = []
rev xs = foldl (\l x -> x:l) [] xs

main = do
      putStrLn(show $ [1, 2, 3, 6, 9, 10, 10030])
      putStrLn("Result: ")
      putStrLn(show $ rev [1, 2, 3, 6, 9, 10, 10030])