check :: (a -> Bool) -> [a] -> Bool
check _ [] = True
check cond (x:xs) | cond x = check cond xs
                  | otherwise = False
                  
main = do
    putStrLn(show $ check (>5) [8,9,3])
    putStrLn(show $ check (>5) [8,9,6])