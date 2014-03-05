import Data.Maybe
import Data.List

findMaxSum :: [Int] -> Int -> Int
findMaxSum [] maxSum = maxSum
findMaxSum (x:xs) maxSum | x > maxSum = findMaxSum xs x
                         | otherwise = findMaxSum xs maxSum
                         
findPosOfMaxSum :: [Int] -> Int    
findPosOfMaxSum [] = 0
findPosOfMaxSum (x:xs) = (fromMaybe (0) . elemIndex (findMaxSum pairSums $ x + head xs) $ pairSums) + 1
          where pairSums = zipWith (+) (x:xs) xs

main = do
      putStrLn(show $ findPosOfMaxSum [1, 5, 6, 2])
      putStrLn(show $ findPosOfMaxSum [1, 5, 1, 7, 6, 200, 6, 2, 200, 6])
      putStrLn(show $ findPosOfMaxSum [1, 5, 1, 7, 6, 1, 6, 2, 200, 100])
      putStrLn(show $ findPosOfMaxSum [])