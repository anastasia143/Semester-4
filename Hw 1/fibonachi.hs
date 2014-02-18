fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n | n > 0 = fib(n - 1) + fib(n - 2) 
      | otherwise = error "Argument should be positive"

main = do
	putStrLn "Enter serial number of desired fibonachi number: "
	n <- readLn
	putStrLn "n! = "
	print (fib n)