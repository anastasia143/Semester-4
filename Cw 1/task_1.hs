primeNumbers = filter isPrime [1..]

isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = helper x (x-1)
    where helper :: Int -> Int -> Bool
          helper _ 1 = True
          helper a b = if (mod a b == 0) then False else helper a (b-1)

main = do
    putStrLn(show $ take 20 primeNumbers)