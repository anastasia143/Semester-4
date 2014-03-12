add :: Int -> [Int] -> [Int]
add val [] = [val]
add val (x:xs) | val <= x = val:x:xs
               | otherwise = (x:(add val xs))
               
remove :: Int -> [Int] -> [Int]
remove val [] = []
remove val (x:xs) | val == x = xs
                  | otherwise = (x:(remove val xs))

doLoop :: [Int] -> IO ()
doLoop list = do
    putStrLn "Enter a command:"
    command <- getLine
    case command of
        '0':_ -> putStrLn("End.")
        '1':_ -> do putStrLn ("Enter value: ")
                    val <- readLn
                    doLoop $ add val list
        '2':_ -> do putStrLn ("Enter value: ")
                    val <- readLn
                    doLoop $ remove val list
        '3':_ -> do putStrLn $ show list
                    doLoop list
        _     -> do putStrLn("Error. Try again.")
                    doLoop list
                               
main = do
     putStrLn "0 - exit"
     putStrLn "1 - add value to sorted list"
     putStrLn "2 - remove value from list"
     putStrLn "3 - print list"
     doLoop []