import Control.Monad

list123 :: Int -> [[Int]]
list123 1 = [[1],[2],[3]]
list123 n = liftM2 (mplus) [[1], [2], [3]] (list123 (n - 1))

main = do
    putStrLn(show $ list123 2)
    putStrLn(show $ list123 3)