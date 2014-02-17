factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n < 0 = error "Argument shold be not negative" | otherwise = n * factorial(n - 1)

main = do
	putStrLn "Enter n: "
	n <- readLn
	putStrLn "n! = "
	print (factorial n)